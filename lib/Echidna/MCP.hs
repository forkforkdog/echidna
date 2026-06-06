{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Echidna.MCP where

import Control.Concurrent (forkIO, threadDelay)
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (forever, unless, void, when)
import Control.Concurrent.STM
import Control.Concurrent.Chan (dupChan, readChan)
import Data.Aeson (FromJSON(..), Result(..), ToJSON(..), Value(..), encode, fromJSON, object, toJSON, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef (readIORef, modifyIORef', newIORef, IORef, atomicModifyIORef', writeIORef)
import Data.Int (Int64)
import Data.List (find, isPrefixOf, isSuffixOf, sort)
import Data.Scientific (floatingOrInteger)
import qualified Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (LocalTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Text.Printf (printf)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)
import System.Directory (getCurrentDirectory)
import System.Timeout (timeout)
import Data.Char (digitToInt, isHexDigit, isSpace, toLower)
import Data.String (fromString)

import MCP.Server
import EVM.Dapp (DappInfo(..))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Types (Addr, W256)
import EVM.ABI (AbiValue(..), AbiType(..), abiValueType)
import Echidna.Types.Test (EchidnaTest(..), TestState(..), TestType(..), didFail, isOptimizationTest)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), maxGasPerBlock)
import Echidna.Types.Coverage (CoverageFileType(..), mergeCoverageMaps, coverageStats)
import Echidna.Output.Source (coverageLineHits, ppCoveredCode, saveLcovHook)
import Echidna.Output.Corpus (loadTxs)

import Echidna.Types.Config (Env(..), EConfig(..), MCPConf(..))
import Echidna.Types.World (World(..))
import Echidna.Types.Campaign
  ( getNFuzzWorkers, CampaignConf(..), WorkerState(..)
  , SampleStats(..), mergeSampleStats
  )
import Echidna.Types.InterWorker (Bus, Message(..), WrappedMessage(..), AgentId(..), FuzzerCmd(..), BroadcastMsg(..))
import Echidna.Types.Worker qualified as Worker
import Network.HTTP.Types (ResponseHeaders, Status, hContentType, methodGet, methodPost, status200, status202, status404, status405, status413)
import Network.Wai (Application, Response, getRequestBodyChunk, pathInfo, requestMethod, responseBuilder, responseLBS)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)

-- | Status state to track coverage info
data StatusState = StatusState
  { lastCoverageTime :: Maybe UTCTime
  , coveredFunctions :: [Text]
  }

-- | MCP Tool Definition
-- Simulates the definition of a tool exposed by an MCP server.
type ToolExecution = [(Text, Text)] -> Env -> Bus -> IO String

data Tool = Tool
  { toolName :: String
  , toolDescription :: String
  , execute :: ToolExecution
  }

-- | Helper to get function name from Tx
getFunctionName :: Tx -> Text
getFunctionName tx = case tx.call of
  SolCall (name, _) -> name
  _ -> "unknown"

-- | Implementation of status tool. Returns a JSON document so agents can
-- chain on individual fields without parsing free-form text.
statusTool :: [IORef WorkerState] -> IORef StatusState -> ToolExecution
statusTool workerRefs statusRef _ env _ = do
  c <- readIORef env.corpusRef
  st <- readIORef statusRef
  now <- getCurrentTime

  workers <- mapM readIORef workerRefs
  let iterations = sum $ map (.ncalls) workers
      maxIterations = env.cfg.campaignConf.testLimit

  (covPoints, _) <- coverageStats env.coverageRefInit env.coverageRefRuntime

  tests <- mapM readIORef env.testRefs
  let failedCount = length $ filter didFail tests
      totalCount = length tests

      optTests = filter isOptimizationTest tests
      optEntry t = object
        [ "type"  .= show t.testType
        , "value" .= show t.value
        ]
      optValues = map optEntry optTests

      timeSinceCov = case st.lastCoverageTime of
        Nothing -> Null
        Just t  -> toJSON (round (diffUTCTime now t) :: Integer)

  let samples = map sampleStatsJson (Map.toList (collectSamples workers))

  pure $ BL8.unpack $ encode $ object
    [ "corpus_size"                  .= Set.size c
    , "iterations"                   .= iterations
    , "iteration_limit"              .= maxIterations
    , "coverage_points"              .= covPoints
    , "tests_failed"                 .= failedCount
    , "tests_total"                  .= totalCount
    , "optimization_values"          .= optValues
    , "time_since_last_coverage_sec" .= timeSinceCov
    , "recent_covered_functions"     .= st.coveredFunctions
    , "samples"                      .= samples
    ]

-- | Helper functions for inject_transaction
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = case break (== c) s of
                (chunk, rest) -> chunk : case rest of
                                           [] -> []
                                           (_:r) -> splitOn c r

splitArgs :: String -> [String]
splitArgs s = go s 0 ""
  where
    go :: String -> Int -> String -> [String]
    go [] _ current = [reverse current]
    go (c:cs) level current
      | c == '[' = go cs (level + 1) (c:current)
      | c == ']' = go cs (level - 1) (c:current)
      | c == ',' && level == 0 = reverse current : go cs level ""
      | otherwise = go cs level (c:current)

parsePrimitive :: String -> Maybe AbiValue
parsePrimitive s =
   let s' = trim s
       lowerS = map toLower s'
   in if lowerS == "true"
      then Just (AbiBool True)
      else if lowerS == "false"
      then Just (AbiBool False)
      else if "0x" `isPrefixOf` s'
           then AbiAddress . fromIntegral <$> (readMaybe s' :: Maybe Integer)
           else AbiUInt 256 . fromIntegral <$> (readMaybe s' :: Maybe Integer)

parseArray :: String -> Maybe AbiValue
parseArray s = do
  let content = trim (drop 1 (take (length s - 1) s))
  let parts = if null content then [] else splitOn ',' content
  vals <- mapM parsePrimitive parts
  let vec = Vector.fromList vals
  if Vector.null vec
    then return $ AbiArrayDynamic (AbiUIntType 256) vec
    else do
      let t = abiValueType (Vector.head vec)
      if all (\v -> abiValueType v == t) vals
        then return $ AbiArrayDynamic t vec
        else Nothing

parseArg :: String -> Maybe AbiValue
parseArg s =
   let s' = trim s
   in if "[" `isPrefixOf` s' && "]" `isSuffixOf` s'
      then parseArray s'
      else parsePrimitive s'

parseFuzzArg :: String -> Maybe (Maybe AbiValue)
parseFuzzArg s =
   let s' = trim s
   in if s' == "?"
      then Just Nothing
      else Just <$> parseArg s'

parseFuzzCall :: String -> Maybe (Text, [Maybe AbiValue])
parseFuzzCall s = do
   let (fname, rest) = break (== '(') s
   if null rest then Nothing else do
     let argsS = take (length rest - 2) (drop 1 rest) -- remove parens
     let argParts = if all isSpace argsS then [] else splitArgs argsS
     args <- mapM parseFuzzArg argParts
     return (pack fname, args)

parseFuzzSequence :: String -> Maybe [(Text, [Maybe AbiValue])]
parseFuzzSequence s = mapM (parseFuzzCall . trim) (splitOn ';' s)

parseCall :: String -> Maybe (String, [AbiValue])
parseCall s = do
   let (fname, rest) = break (== '(') s
   if null rest then Nothing else do
     let argsS = take (length rest - 2) (drop 1 rest) -- remove parens
     let argParts = if all isSpace argsS then [] else splitArgs argsS
     args <- mapM parseArg argParts
     return (fname, args)

readAddr :: String -> Maybe Addr
readAddr s = fromIntegral <$> (readMaybe s :: Maybe Integer)

parseTx :: Maybe Tx -> String -> Maybe Tx
parseTx ctx s = do
   let parts = words s
   case parts of
     (srcS:dstS:valS:_:_) | length parts >= 4 -> do
         src <- readAddr srcS
         dst <- readAddr dstS
         val <- readMaybe valS
         (fname, args) <- parseCall (unwords (drop 3 parts))
         return $ Tx (SolCall (pack fname, args)) src dst 1000000 0 val (0,0)
     _ -> do
         (fname, args) <- parseCall s
         let (src, dst) = case ctx of
               Just t -> (t.src, t.dst)
               Nothing -> (0x1000, 0x2000)
         return $ Tx (SolCall (pack fname, args)) src dst 1000000 0 0 (0,0)

-- | Implementation of reload_corpus tool
reloadCorpusTool :: ToolExecution
reloadCorpusTool _ env _ = do
  dir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
  loadedSeqs <- loadTxs dir -- returns [(FilePath, [Tx])]

  if null loadedSeqs
    then return "No transaction sequences found in corpus directory."
    else do
      currentCorpus <- readIORef env.corpusRef
      let existingTxs = Set.map snd currentCorpus

      let newSeqs = map snd loadedSeqs
      let uniqueNewSeqs = filter (`Set.notMember` existingTxs) newSeqs

      if null uniqueNewSeqs
        then return "No NEW transaction sequences found in corpus directory."
        else do
             let maxId = if Set.null currentCorpus
                         then 0
                         else fst (Set.findMax currentCorpus)

             let indexedNewSeqs = zip [maxId + 1 ..] uniqueNewSeqs
             let newCorpus = Set.union currentCorpus (Set.fromList indexedNewSeqs)

             atomicModifyIORef' env.corpusRef $ const (newCorpus, ())
             return $ printf "Reloaded %d new transaction sequences from %s" (length uniqueNewSeqs) dir

-- | Implementation of dump_lcov tool
dumpLcovTool :: ToolExecution
dumpLcovTool _ env _ = do
  let contracts = Map.elems env.dapp.solcByName
  dir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
  filename <- saveLcovHook env dir env.sourceCache contracts
  return $ "Dumped LCOV coverage to " ++ filename

-- | Implementation of inject_fuzz_transactions tool
fuzzTransactionTool :: ToolExecution
fuzzTransactionTool args env bus = do
  let txStr = Data.Maybe.fromMaybe "" (lookup "transactions" args)
  case parseFuzzSequence (unpack txStr) of
    Nothing -> return "Error: Failed to parse transaction sequence string."
    Just seqPrototype -> do
      -- Validate function names and argument counts
      let dapp = env.dapp
          methods = Map.elems dapp.abiMap

          methodsByName = Map.fromListWith (++) [(m.name, [m]) | m <- methods]

          validateCall (name, callArgs) =
            case Map.lookup name methodsByName of
              Nothing -> Just $ printf "Function '%s' not found." (unpack name)
              Just ms ->
                if any (\m -> length m.inputs == length callArgs) ms
                then Nothing
                else Just $ printf "Function '%s' found but with different argument count. Expected: %s, Got: %d" (unpack name) (show $ map (length . (.inputs)) ms) (length callArgs)

          errors = Data.Maybe.mapMaybe validateCall seqPrototype

      if not (null errors)
        then return $ "Error:\n" ++ unlines errors
        else do
          let nWorkers = getNFuzzWorkers env.cfg.campaignConf
              calcProb i
                -- Worker 0 always injects transactions at position 0 with a probability of 90%
                | i == 0 = 0.9
                -- For small campaigns (<= 2 workers), all workers share a low probability (20%)
                | nWorkers <= 2 = 0.2
                -- For larger campaigns, scale probability linearly from 20% to 90% for other workers
                | otherwise = 0.2 + fromIntegral (i - 1) * (0.7 / fromIntegral (nWorkers - 2))

          mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i (FuzzSequence seqPrototype (calcProb i))))) [0 .. nWorkers - 1]
          return $ printf "Requested fuzzing of transaction sequence '%s' on %d fuzzers" (unpack txStr) nWorkers

-- | Implementation of clear_fuzz_priorities tool
clearPrioritiesTool :: ToolExecution
clearPrioritiesTool _ env bus = do
  let nWorkers = getNFuzzWorkers env.cfg.campaignConf
  mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i ClearPrioritization))) [0 .. nWorkers - 1]
  return $ printf "Requested clearing priorities on %d fuzzers" nWorkers

-- | Implementation of sample tool. The single argument @function@ is either:
--   * the literal "off" to clear all sampling, or
--   * a function name or canonical signature ("totalSupply", "totalSupply()",
--     "transfer(address,uint256)") which is resolved against the contract's
--     ABI and broadcast to every worker.
sampleTool :: ToolExecution
sampleTool args env bus = do
  let nWorkers = getNFuzzWorkers env.cfg.campaignConf
      input   = T.strip (Data.Maybe.fromMaybe "" (lookup "function" args))
  if T.toLower input == "off"
     then do
       mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i ClearSampling))) [0 .. nWorkers - 1]
       pure "Sampling cleared on all fuzzers."
     else if T.null input
       then pure "Error: 'function' argument is required (or pass 'off' to clear sampling)."
       else do
         let allMethods = concatMap (Map.elems . (.abiMap)) (Map.elems env.dapp.solcByName)
             matchesInput m = m.methodSignature == input || m.name == input
             matchedSigs = Set.fromList $ map (.methodSignature) (filter matchesInput allMethods)
         case Set.toList matchedSigs of
           []    -> pure $ printf "Error: function '%s' not found in contract ABI." (unpack input)
           [sig] -> do
             mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i (EnableSampling sig)))) [0 .. nWorkers - 1]
             pure $ printf "Sampling enabled for %s on %d fuzzers." (unpack sig) nWorkers
           sigs  ->
             pure $ printf "Error: '%s' is ambiguous, matches %s. Pass the full signature."
                       (unpack input) (unpack $ T.intercalate ", " sigs)

-- | Merge all workers' 'sampledFunctions' maps into a single map.
collectSamples :: [WorkerState] -> Map Text SampleStats
collectSamples = Map.unionsWith mergeSampleStats . map (.sampledFunctions)

-- | Render one merged 'SampleStats' as a JSON object for inclusion in
-- 'statusTool' output.
sampleStatsJson :: (Text, SampleStats) -> Value
sampleStatsJson (sig, st) = object
  [ "function"       .= sig
  , "calls"          .= st.sampleCalls
  , "reverts"        .= st.sampleReverts
  , "return_range"   .= case st.sampleReturnRange of
      Nothing       -> Null
      Just (lo, hi) -> object [ "min" .= show lo, "max" .= show hi ]
  , "recent_reverts" .= st.sampleRecentReverts
  ]

-- | Parse a transaction-sequence string into a list of concrete 'Tx' values.
-- Rejects wildcards ('?') because both execute_sequence and trace_sequence
-- require fully concrete arguments.
parseAndBuildTxs :: Env -> Text -> Either String [Tx]
parseAndBuildTxs env txStr =
  case parseFuzzSequence (unpack txStr) of
    Nothing -> Left "Error: Failed to parse transaction sequence string."
    Just seqPrototype
      | any (\(_, callArgs) -> any Data.Maybe.isNothing callArgs) seqPrototype ->
          Left "Error: concrete arguments required (no '?' wildcards)."
      | otherwise ->
          let dapp = env.dapp
              methods = Map.elems dapp.abiMap
              methodsByName = Map.fromListWith (++) [(m.name, [m]) | m <- methods]

              validateCall (name, callArgs) =
                case Map.lookup name methodsByName of
                  Nothing -> Just $ printf "Function '%s' not found." (unpack name)
                  Just ms ->
                    if any (\m -> length m.inputs == length callArgs) ms
                    then Nothing
                    else Just $ printf "Function '%s' found but with different argument count. Expected: %s, Got: %d"
                                       (unpack name)
                                       (show $ map (length . (.inputs)) ms)
                                       (length callArgs)

              errors = Data.Maybe.mapMaybe validateCall seqPrototype
          in if not (null errors)
             then Left $ "Error:\n" ++ unlines errors
             else
               let solConf = env.cfg.solConf
                   dst = solConf.contractAddr
                   src = case Set.toList solConf.sender of
                           (s:_) -> s
                           []    -> 0x10000

                   mkTx (name, callArgs) =
                     let concreteArgs = Data.Maybe.catMaybes callArgs
                     in Tx { call = SolCall (name, concreteArgs)
                           , src = src
                           , dst = dst
                           , gas = fromIntegral maxGasPerBlock
                           , gasprice = 0
                           , value = 0
                           , delay = (0 :: W256, 0 :: W256)
                           }
               in Right (map mkTx seqPrototype)

-- | Send a request to worker 0 only and wait for its reply with a timeout.
askWorkerZero :: Bus -> (TMVar String -> FuzzerCmd) -> Int -> IO (Maybe String)
askWorkerZero bus mkCmd timeoutMicros = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer 0 (mkCmd replyVar)))
  timeout timeoutMicros (atomically $ takeTMVar replyVar)

-- | Parse a "true"/"false" string argument (case insensitive); anything else
-- including absence is treated as 'False'.
parseBoolArg :: Text -> Bool
parseBoolArg t = T.toLower t == "true"

-- | Implementation of execute_sequence tool. Concrete replay on worker 0;
-- no random noise, no campaign side effects. Returns a JSON report. If the
-- optional @trace@ argument is "true", the report additionally carries a
-- @trace@ field with the EVM trace tree of the LAST tx (intermediate trees
-- are skipped because 'showTraceTree' is expensive).
executeSequenceTool :: ToolExecution
executeSequenceTool args env bus = do
  let txStr = Data.Maybe.fromMaybe "" (lookup "transactions" args)
      withTrace = parseBoolArg (Data.Maybe.fromMaybe "" (lookup "trace" args))
  case parseAndBuildTxs env txStr of
    Left err -> return err
    Right txs -> do
      result <- askWorkerZero bus (ExecuteSequence txs withTrace) 300_000_000
      return $ Data.Maybe.fromMaybe
        "Error: Timeout waiting for execute_sequence result (300s)."
        result

-- | Implementation of target tool
targetTool :: ToolExecution
targetTool _ env _ = do
  let contracts = env.dapp.solcByName
      world = env.world

      -- Helper to check if a contract is a target
      isTarget :: SolcContract -> Bool
      isTarget c = c.runtimeCodehash `Map.member` world.highSignatureMap

      -- Find candidates
      candidates = filter (isTarget . snd) (Map.toList contracts)

  case candidates of
    [] -> return "Error: No target contract found."
    ((name, contract):_) -> do
      let signatures = map (.methodSignature) (Map.elems contract.abiMap)
          sortedSigs = sort signatures
      return $ printf "Contract: %s\nFunctions:\n- %s" (unpack name) (unpack $ T.intercalate "\n- " sortedSigs)


-- | Implementation of show_coverage tool
showCoverageTool :: ToolExecution
showCoverageTool args env _ = do
  let contractName = Data.Maybe.fromMaybe "" (lookup "contract" args)
  if T.null contractName
     then return "Error: No contract name provided"
     else do
       let dapp = env.dapp
       let matches = Map.filterWithKey (\k _ -> k == contractName || (":" <> contractName) `T.isSuffixOf` k) dapp.solcByName
       case Map.toList matches of
         [] -> return $ printf "Error: Contract '%s' not found" (unpack contractName)
         [(k, solc)] -> do
            covMap <- mergeCoverageMaps dapp env.coverageRefInit env.coverageRefRuntime
            let sc = env.sourceCache

            -- Identify relevant files: only the file defining the contract
            let relevantFiles = Set.singleton $ unpack $ T.dropEnd 1 $ fst $ T.breakOnEnd ":" k

            -- Use all active contracts to generate coverage
            -- This allows showing coverage for a parent contract (e.g. EchidnaTest)
            -- derived from the execution of a child contract (e.g. Echidna).
            let activeContracts = filter (\c -> c.runtimeCodehash `Map.member` covMap) (Map.elems dapp.solcByName)
            -- If no contracts are active (e.g. no coverage yet), use the requested contract to at least show the source
            let contractsToUse = if null activeContracts then [solc] else activeContracts

            -- Generate full report using all active contracts, then filter by relevant files
            let fullReport = ppCoveredCode Txt sc contractsToUse covMap Nothing "" []
            let filterReport text =
                  let ls = T.lines text
                      splitSections [] = []
                      splitSections (l:rest) =
                          let (content, next) = span (" " `T.isPrefixOf`) rest
                          in (l:content) : splitSections next
                      sections = splitSections ls
                      keepSection (header:content) =
                          if unpack header `Set.member` relevantFiles
                          then header : content
                          else []
                      keepSection [] = []
                  in T.unlines $ concatMap keepSection sections

            return $ "```\n" ++ unpack (filterReport fullReport) ++ "\n```"
         candidates -> return $ printf "Error: Ambiguous contract name '%s'. Found: %s" (unpack contractName) (unpack $ T.intercalate ", " $ map fst candidates)

-- | Registry of available tools
availableTools :: [IORef WorkerState] -> IORef StatusState -> [Tool]
availableTools workerRefs statusRef =
  [ Tool "status" "Return fuzzing campaign status as a JSON document with fields: corpus_size, iterations, iteration_limit, coverage_points, tests_failed, tests_total, optimization_values, time_since_last_coverage_sec, recent_covered_functions" (statusTool workerRefs statusRef)
  , Tool "target" "Show the name and the ABI of the target contract" targetTool
  , Tool "reload_corpus" "Reload the transactions from the corpus, but without replay them" reloadCorpusTool
  , Tool "dump_lcov" "Dump coverage in LCOV format" dumpLcovTool
  , Tool "inject_fuzz_transactions" "Inject a sequence of transaction to fuzz with optional concrete arguments" fuzzTransactionTool
  , Tool "execute_sequence" "Replay a concrete transaction sequence on worker 0 (no random noise) and return a JSON report. Pass trace=\"true\" to also include the EVM trace tree of the LAST tx in the report." executeSequenceTool
  , Tool "clear_fuzz_priorities" "Clear the function prioritization list used in fuzzing" clearPrioritiesTool
  , Tool "sample" "Enable sampling for a function (e.g. 'totalSupply' or 'transfer(address,uint256)') to track call/revert counts, recent revert summaries and return-value min/max ranges. Pass 'off' to clear all sampling. Results appear in the 'samples' field of the 'status' tool output." sampleTool
  , Tool "show_coverage" "Show coverage report for a particular contract" showCoverageTool
  ]

-- | Run the MCP Server
runMCPServer :: Env -> [IORef WorkerState] -> Int -> IO ()
runMCPServer env workerRefs port = do
    statusRef <- newIORef (StatusState Nothing [])

    -- Spawn listener for coverage events
    myBus <- atomically $ dupTChan env.bus
    _ <- forkIO $ forever $ do
      msg <- atomically $ readTChan myBus
      case msg of
        WrappedMessage _ (Broadcast (NewCoverageInfo _ txs isReplaying)) -> do
           unless isReplaying $ do
               now <- getCurrentTime
               let funcNames = map getFunctionName txs
                   lastFunc = if null funcNames then "unknown" else last funcNames

               modifyIORef' statusRef $ \st -> st
                 { lastCoverageTime = Just now
                 , coveredFunctions = take 10 (lastFunc : st.coveredFunctions)
                 }
        _ -> return ()

    let toolsList = availableTools workerRefs statusRef

    let httpConfig = HttpConfig
            { httpPort = port
            , httpHost = "127.0.0.1"
            , httpEndpoint = "/mcp"
            , httpVerbose = False
            }

    let serverInfo = McpServerInfo
            { serverName = "Echidna MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "Echidna Agent Interface. Available tools: status, target, reload_corpus, dump_lcov, inject_fuzz_transactions, execute_sequence, clear_fuzz_priorities, sample, show_coverage"
            }

    let mkToolDefinition :: Tool -> ToolDefinition
        mkToolDefinition t = ToolDefinition
            { toolDefinitionName = pack t.toolName
            , toolDefinitionDescription = pack t.toolDescription
            , toolDefinitionInputSchema = case t.toolName of
                "dump_lcov" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "target" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "inject_fuzz_transactions" -> InputSchemaDefinitionObject
                    { properties = [("transactions", InputSchemaDefinitionProperty "string" "The transaction sequence string separated by ';' (e.g. 'func1();func2(arg1, ?)')")]
                    , required = ["transactions"]
                    }
                "execute_sequence" -> InputSchemaDefinitionObject
                    { properties =
                        [ ("transactions", InputSchemaDefinitionProperty "string" "Concrete transaction sequence separated by ';' (no '?' wildcards), e.g. 'supply(1000);borrow(500)'")
                        , ("trace",        InputSchemaDefinitionProperty "string" "Optional: \"true\" to include the EVM trace tree of the LAST tx in the JSON report. Defaults to \"false\".")
                        ]
                    , required = ["transactions"]
                    }
                "clear_fuzz_priorities" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "sample" -> InputSchemaDefinitionObject
                    { properties = [("function", InputSchemaDefinitionProperty "string" "Function name or canonical signature to sample (e.g. 'totalSupply', 'transfer(address,uint256)'). Pass 'off' to clear all sampling.")]
                    , required = ["function"]
                    }
                "show_coverage" -> InputSchemaDefinitionObject
                    { properties = [("contract", InputSchemaDefinitionProperty "string" "The name of the contract")]
                    , required = ["contract"]
                    }
                "status" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "reload_corpus" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                _ -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
            , toolDefinitionTitle = Nothing
            , toolDefinitionMeta = Nothing
            }

    let toolDefs = map mkToolDefinition toolsList

    let handleToolCall :: ToolName -> [(ArgumentName, ArgumentValue)] -> IO (Either Error Content)
        handleToolCall name args = do
            case find (\t -> pack t.toolName == name) toolsList of
                Nothing -> return $ Left $ UnknownTool name
                Just tool -> do
                    result <- tool.execute args env env.bus
                    return $ Right $ ContentText $ pack result

    let handlers = McpServerHandlers
            { prompts = Nothing
            , resources = Nothing
            , tools = Just (return toolDefs, handleToolCall)
            }

    runMcpServerHttpWithConfig httpConfig serverInfo handlers

data StreamableEventBuffer = StreamableEventBuffer
  { streamableNextEventId :: !Int
  , streamableEventItems  :: ![(Int, StreamableMCPEvent)]
  }

data StreamableMCPState = StreamableMCPState
  { streamableEventsRef :: IORef StreamableEventBuffer
  , streamableStartedAt :: UTCTime
  , streamableMaxEvents :: Int
  , streamableMaxRequestBytes :: Int64
  , streamableCoverageRef :: IORef Value
  , streamableCoverageRefreshRef :: IORef Bool
  }

data StreamableMCPEvent = StreamableMCPEvent
  { streamableEventId :: !Int
  , streamableTimestamp :: !Text
  , streamableWorkerId :: !(Maybe Int)
  , streamableWorkerType :: !(Maybe Text)
  , streamableEventType :: !Text
  , streamablePayload :: !Value
  }

instance ToJSON StreamableMCPEvent where
  toJSON ev = object
    [ "id" .= ev.streamableEventId
    , "ts" .= ev.streamableTimestamp
    , "workerId" .= ev.streamableWorkerId
    , "workerType" .= ev.streamableWorkerType
    , "type" .= ev.streamableEventType
    , "payload" .= ev.streamablePayload
    ]

data StreamableRpcRequest = StreamableRpcRequest
  { streamableRpcId :: Maybe Value
  , streamableRpcMethod :: Text
  , streamableRpcParams :: Maybe Value
  }

instance FromJSON StreamableRpcRequest where
  parseJSON = withObject "StreamableRpcRequest" $ \o ->
    StreamableRpcRequest <$> o .:? "id" <*> o .: "method" <*> o .:? "params"

runStreamableMCPServer :: Env -> [IORef WorkerState] -> IO ()
runStreamableMCPServer env workerRefs = do
  started <- getCurrentTime
  eventsRef <- newIORef (StreamableEventBuffer 0 [])
  coverageRef <- newIORef streamableEmptyCoverageLines
  coverageRefreshRef <- newIORef False
  let conf = env.cfg.mcpConf
      st = StreamableMCPState eventsRef started (max 1 conf.maxEvents) (fromIntegral $ max 1 conf.maxRequestBytes) coverageRef coverageRefreshRef
  ch <- dupChan env.eventQueue
  _ <- forkIO $ forever $ do
    (ts, ev) <- readChan ch
    recordStreamableEvent conf st ts ev
    streamableMaybeRefreshCoverage env st ev
  let settings = setPort conf.port . setHost (fromString $ T.unpack conf.host) $ defaultSettings
  runSettings settings (streamableMCPApp env workerRefs st)

streamableMCPApp :: Env -> [IORef WorkerState] -> StreamableMCPState -> Application
streamableMCPApp env workerRefs st req respond =
  case (requestMethod req, pathInfo req) of
    (m, ["health"]) | m == methodGet ->
      respond $ responseLBS status200 jsonHeaders "{\"ok\":true}"
    (m, ["mcp"]) | m == methodPost -> do
      bodyResult <- streamableStrictRequestBodyWithLimit st.streamableMaxRequestBytes (getRequestBodyChunk req)
      case bodyResult of
        Left _ ->
          respond $ streamableJson status413 $ streamableMcpError Null (-32000) "Request body too large"
        Right body ->
          case eitherDecodeValue body of
            Left err ->
              respond $ streamableJson status200 $ streamableMcpError Null (-32700) (T.pack err)
            Right val ->
              case fromJSON val of
                Error err ->
                  respond $ streamableJson status200 $ streamableMcpError Null (-32600) (T.pack err)
                Success rpc -> do
                  result <- handleStreamableRpc env workerRefs st rpc
                  case result of
                    Nothing -> respond $ responseBuilder status202 jsonHeaders mempty
                    Just val' -> respond $ streamableJson status200 val'
    (_, ["mcp"]) ->
      respond $ responseLBS status405 jsonHeaders "{\"error\":\"method not allowed\"}"
    _ ->
      respond $ responseLBS status404 jsonHeaders "{\"error\":\"not found\"}"
  where
    eitherDecodeValue = Aeson.eitherDecode

streamableStrictRequestBodyWithLimit :: Int64 -> IO BS.ByteString -> IO (Either Int64 BL.ByteString)
streamableStrictRequestBodyWithLimit limit nextChunk = go 0 []
  where
    go !bytes chunks = do
      chunk <- nextChunk
      if BS.null chunk
        then pure . Right . BL.fromChunks $ reverse chunks
        else do
          let !bytes' = bytes + fromIntegral (BS.length chunk)
          if bytes' > limit
            then pure (Left bytes')
            else go bytes' (chunk : chunks)

streamableJson :: Status -> Value -> Response
streamableJson status val = responseLBS status jsonHeaders (encode val)

jsonHeaders :: ResponseHeaders
jsonHeaders =
  [ (hContentType, "application/json")
  , ("Mcp-Session-Id", "echidna")
  ]

handleStreamableRpc :: Env -> [IORef WorkerState] -> StreamableMCPState -> StreamableRpcRequest -> IO (Maybe Value)
handleStreamableRpc env workerRefs st req =
  case req.streamableRpcMethod of
    "initialize" ->
      pure . Just $ streamableMcpResult (fromMaybeNull req.streamableRpcId) streamableInitializeResult
    "notifications/initialized" ->
      pure Nothing
    "ping" ->
      pure . Just $ streamableMcpResult (fromMaybeNull req.streamableRpcId) (object [])
    "tools/list" ->
      pure . Just $ streamableMcpResult (fromMaybeNull req.streamableRpcId) streamableToolsList
    "resources/list" ->
      pure . Just $ streamableMcpResult (fromMaybeNull req.streamableRpcId) streamableResourcesList
    "tools/call" -> do
      res <- streamableToolsCall env workerRefs st req.streamableRpcParams
      pure . Just $ either id (streamableMcpResult (fromMaybeNull req.streamableRpcId)) res
    "resources/read" -> do
      res <- streamableResourcesRead env workerRefs st req.streamableRpcParams
      pure . Just $ either id (streamableMcpResult (fromMaybeNull req.streamableRpcId)) res
    _ ->
      pure . Just $ streamableMcpError (fromMaybeNull req.streamableRpcId) (-32601) "Method not found"

streamableInitializeResult :: Value
streamableInitializeResult = object
  [ "protocolVersion" .= ("2024-11-05" :: Text)
  , "capabilities" .= object
      [ "resources" .= object []
      , "tools" .= object []
      ]
  , "serverInfo" .= object
      [ "name" .= ("echidna" :: Text)
      , "title" .= ("Echidna" :: Text)
      , "version" .= ("1.0.0" :: Text)
      ]
  ]

streamableToolsList :: Value
streamableToolsList = object
  [ "tools" .=
      [ streamableTool "get_status" "Get run status"
      , streamableTool "get_events" "Get recent campaign events"
      , streamableTool "get_reproducers" "List reproducer snapshots"
      , streamableTool "get_reproducer" "Get a reproducer snapshot"
      , streamableTool "get_coverage_hits" "Get coverage line hits"
      ]
  ]
  where
    streamableTool name desc = object
      [ "name" .= (name :: Text)
      , "description" .= (desc :: Text)
      , "inputSchema" .= object []
      ]

streamableResourcesList :: Value
streamableResourcesList = object
  [ "resources" .=
      [ streamableResource "echidna://run/status" "Run status"
      , streamableResource "echidna://run/events" "Events"
      , streamableResource "echidna://run/reproducers" "Reproducer snapshots"
      , streamableResource "echidna://run/reproducer/<test-key>" "Single reproducer snapshot"
      , streamableResource "echidna://coverage/lines" "Coverage line hits"
      ]
  ]
  where
    streamableResource uri name = object
      [ "uri" .= (uri :: Text)
      , "name" .= (name :: Text)
      , "mimeType" .= ("application/json" :: Text)
      ]

streamableToolsCall :: Env -> [IORef WorkerState] -> StreamableMCPState -> Maybe Value -> IO (Either Value Value)
streamableToolsCall env workerRefs st params =
  case params of
    Just (Object o) ->
      case KM.lookup (K.fromText "name") o of
        Just (String name) -> do
          let args = KM.lookup (K.fromText "arguments") o
          value <- runStreamableTool env workerRefs st name args
          pure . Right . object $
            [ "content" .=
                [ object
                    [ "type" .= ("text" :: Text)
                    , "text" .= decodeUtf8 (BL8.toStrict $ encode value)
                    ]
                ]
            ]
        _ -> pure . Left $ streamableMcpError Null (-32602) "Missing tool name"
    _ -> pure . Left $ streamableMcpError Null (-32602) "Missing params"

streamableResourcesRead :: Env -> [IORef WorkerState] -> StreamableMCPState -> Maybe Value -> IO (Either Value Value)
streamableResourcesRead env workerRefs st params =
  case params of
    Just (Object o) ->
      case KM.lookup (K.fromText "uri") o of
        Just (String uri) -> do
          value <- readStreamableResource env workerRefs st uri
          let content = object
                [ "uri" .= uri
                , "mimeType" .= ("application/json" :: Text)
                , "text" .= decodeUtf8 (BL8.toStrict $ encode value)
                ]
          pure . Right $ object ["contents" .= [content]]
        _ -> pure . Left $ streamableMcpError Null (-32602) "Missing uri"
    _ -> pure . Left $ streamableMcpError Null (-32602) "Missing params"

runStreamableTool :: Env -> [IORef WorkerState] -> StreamableMCPState -> Text -> Maybe Value -> IO Value
runStreamableTool env workerRefs st name args =
  case name of
    "get_status" -> streamableStatus env workerRefs st
    "get_events" -> streamableEvents st (streamableArgsToMap args)
    "get_reproducers" -> streamableReproducers env (streamableArgsToMap args)
    "get_reproducer" -> streamableReproducer env (streamableArgsToMap args)
    "get_coverage_hits" -> streamableCoverageLines env st
    _ -> pure $ object ["error" .= ("unknown tool" :: Text)]

readStreamableResource :: Env -> [IORef WorkerState] -> StreamableMCPState -> Text -> IO Value
readStreamableResource env workerRefs st uri =
  case streamableSplitUri uri of
    ("echidna://run/status", _) -> streamableStatus env workerRefs st
    ("echidna://run/events", query) -> streamableEvents st query
    ("echidna://run/reproducers", query) -> streamableReproducers env query
    ("echidna://coverage/lines", _) -> streamableCoverageLines env st
    (path, _) | "echidna://run/reproducer/" `T.isPrefixOf` path ->
      streamableReproducer env (Map.singleton "testKey" (streamableDecodeUriComponent $ T.drop (T.length "echidna://run/reproducer/") path))
    _ -> pure $ object ["error" .= ("unknown resource" :: Text)]

streamableStatus :: Env -> [IORef WorkerState] -> StreamableMCPState -> IO Value
streamableStatus env workerRefs st = do
  workers <- mapM readIORef workerRefs
  tests <- mapM readIORef env.testRefs
  corpus <- readIORef env.corpusRef
  (points, codehashes) <- coverageStats env.coverageRefInit env.coverageRefRuntime
  now <- getCurrentTime
  let failedTests = length $ filter didFail tests
      corpusSize = Set.size corpus
  pure $ streamableStatusSnapshot st.streamableStartedAt now workers failedTests (length tests) points codehashes corpusSize

streamableStatusSnapshot :: UTCTime -> UTCTime -> [WorkerState] -> Int -> Int -> Int -> Int -> Int -> Value
streamableStatusSnapshot startedAt now workers failedTests totalTests points codehashes corpusSize =
  let rawRuns = sum $ map (.ncalls) workers
      runs = max rawRuns failedTests
      successCalls = max 0 (runs - failedTests)
      elapsedMs = floor (realToFrac (diffUTCTime now startedAt) * (1000 :: Double))
  in object
    [ "phase" .= ("running" :: Text)
    , "runs" .= runs
    , "counters" .= object
        [ "totalCalls" .= runs
        , "successCalls" .= successCalls
        , "failedCalls" .= failedTests
        ]
    , "coveragePoints" .= points
    , "uniqueCodehashes" .= codehashes
    , "corpusSize" .= corpusSize
    , "tests" .= object
        [ "total" .= totalTests
        , "failed" .= failedTests
        ]
    , "corpus" .= object ["size" .= corpusSize]
    , "elapsedMs" .= (max 0 elapsedMs :: Int)
    ]

streamableEvents :: StreamableMCPState -> Map Text Text -> IO Value
streamableEvents st query = do
  StreamableEventBuffer _ items <- readIORef st.streamableEventsRef
  let since = streamableReadQueryInt "since" (-1) query
      limit = min st.streamableMaxEvents . max 0 $ streamableReadQueryInt "limit" 200 query
      selected = take limit [ev | (i, ev) <- items, i > since]
  pure $ object ["events" .= selected]

streamableReproducers :: Env -> Map Text Text -> IO Value
streamableReproducers env query = do
  tests <- mapM readIORef env.testRefs
  let offset = max 0 $ streamableReadQueryInt "offset" 0 query
      maxArtifacts = max 0 env.cfg.mcpConf.maxReproducerArtifacts
      requestedLimit = streamableReadQueryInt "limit" maxArtifacts query
      limit = min maxArtifacts (max 0 requestedLimit)
      artifacts = filter streamableArtifactHasReproducer (zipWith (streamableTestArtifact env.cfg.mcpConf) [0..] tests)
      selected = take limit (drop offset artifacts)
  pure $ object
    [ "reproducers" .= selected
    , "count" .= length artifacts
    , "nextOffset" .= (offset + length selected)
    ]

streamableReproducer :: Env -> Map Text Text -> IO Value
streamableReproducer env query = do
  tests <- mapM readIORef env.testRefs
  let artifacts = zipWith (streamableTestArtifact env.cfg.mcpConf) [0..] tests
      matchKey artifact = Map.lookup "testKey" query == streamableObjectText "testKey" artifact
  case find matchKey artifacts of
    Just artifact -> pure $ object ["testKey" .= streamableObjectText "testKey" artifact, "artifact" .= artifact]
    Nothing -> pure $ object ["error" .= ("reproducer not found" :: Text)]

streamableCoverageTimeoutUsec :: Int
streamableCoverageTimeoutUsec = 30000000

streamableCoveragePollUsec :: Int
streamableCoveragePollUsec = 100000

streamableEmptyCoverageLines :: Value
streamableEmptyCoverageLines = object []

streamableCoverageLines :: Env -> StreamableMCPState -> IO Value
streamableCoverageLines env st = do
  cached <- readIORef st.streamableCoverageRef
  if not (streamableIsEmptyCoverageLines cached)
    then pure cached
    else do
      streamableStartCoverageRefresh env st
      streamableWaitForCoverageSnapshot st

streamableWaitForCoverageSnapshot :: StreamableMCPState -> IO Value
streamableWaitForCoverageSnapshot st = do
  waited <- timeout streamableCoverageTimeoutUsec waitLoop
  case waited of
    Just snapshot -> pure snapshot
    Nothing -> readIORef st.streamableCoverageRef
  where
    waitLoop = do
      cached <- readIORef st.streamableCoverageRef
      refreshing <- readIORef st.streamableCoverageRefreshRef
      if not (streamableIsEmptyCoverageLines cached) || not refreshing
        then pure cached
        else threadDelay streamableCoveragePollUsec >> waitLoop

streamableComputeCoverageLines :: Env -> IO Value
streamableComputeCoverageLines env = do
  covMap <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
  let contracts = Map.elems env.dapp.solcByName
      hits = coverageLineHits env.sourceCache covMap contracts env.cfg.campaignConf.coverageExcludes
      snapshot = toJSON hits
  BL8.length (encode snapshot) `seq` pure snapshot

streamableStartCoverageRefresh :: Env -> StreamableMCPState -> IO ()
streamableStartCoverageRefresh env st = do
  shouldStart <- atomicModifyIORef' st.streamableCoverageRefreshRef $ \running ->
    if running then (running, False) else (True, True)
  when shouldStart $ void $ forkIO $
    (do
      computed <- try (streamableComputeCoverageLines env) :: IO (Either SomeException Value)
      case computed of
        Right snapshot -> writeIORef st.streamableCoverageRef snapshot
        _ -> pure ())
    `finally` writeIORef st.streamableCoverageRefreshRef False

streamableMaybeRefreshCoverage :: Env -> StreamableMCPState -> Worker.CampaignEvent -> IO ()
streamableMaybeRefreshCoverage env st = \case
  Worker.WorkerEvent _ _ Worker.NewCoverage{} -> streamableStartCoverageRefresh env st
  _ -> pure ()

streamableIsEmptyCoverageLines :: Value -> Bool
streamableIsEmptyCoverageLines = \case
  Object o -> KM.null o
  _ -> False

streamableTestArtifact :: MCPConf -> Int -> EchidnaTest -> Value
streamableTestArtifact conf idx test =
  let originalLength = length test.reproducer
      shaped = streamableShapeTxs conf test.reproducer
      countTruncated = length shaped < originalLength
      key = T.intercalate ":" ["test", T.pack (show idx), streamableRenderTestType test.testType]
      testId = streamableRenderTestId test
      mkArtifact artifactTxs artifactBytesTruncated =
        let truncated = countTruncated || artifactBytesTruncated || length artifactTxs < length shaped
        in object
          [ "testKey" .= key
          , "testId" .= testId
          , "workerId" .= test.workerId
          , "testType" .= streamableRenderTestType test.testType
          , "state" .= streamableRenderTestState test.state
          , "reproducer" .= object
              [ "latest" .= artifactTxs
              , "best" .= artifactTxs
              , "candidate" .= artifactTxs
              , "length" .= object
                  [ "latest" .= length artifactTxs
                  , "best" .= length artifactTxs
                  , "candidate" .= length artifactTxs
                  ]
              , "originalLength" .= originalLength
              , "returnedLength" .= length artifactTxs
              , "truncated" .= truncated
              , "maxJsonBytes" .= conf.maxReproducerJsonBytes
              ]
          , "shrink" .= object
              [ "status" .= streamableShrinkStatus test.state
              , "fullyShrunk" .= streamableFullyShrunk test.state
              ]
          ]
      (txs, bytesTruncated) = streamableBoundTxsByJsonBytes conf mkArtifact shaped
  in mkArtifact txs bytesTruncated

streamableBoundTxsByJsonBytes :: MCPConf -> ([Tx] -> Bool -> Value) -> [Tx] -> ([Tx], Bool)
streamableBoundTxsByJsonBytes conf mkValue txs
  | conf.maxReproducerJsonBytes <= 0 = (txs, False)
  | withinBudget txs False = (txs, False)
  | otherwise = shrink txs
  where
    maxBytes = fromIntegral conf.maxReproducerJsonBytes
    withinBudget xs truncated = BL8.length (encode (mkValue xs truncated)) <= maxBytes
    shrink [] = ([], True)
    shrink xs =
      let xs' = init xs
      in if null xs' || withinBudget xs' True
           then (xs', True)
           else shrink xs'

streamableEventTestPayload :: MCPConf -> EchidnaTest -> Value
streamableEventTestPayload conf test =
  let originalLength = length test.reproducer
      shaped = streamableShapeTxsLimit conf conf.reproducerEventsLimit test.reproducer
      countTruncated = length shaped < originalLength
      mkPayload payloadTxs payloadBytesTruncated =
        let truncated = countTruncated || payloadBytesTruncated || length payloadTxs < length shaped
        in object
          [ "state" .= test.state
          , "type" .= test.testType
          , "value" .= test.value
          , "reproducer" .= payloadTxs
          , "result" .= test.result
          , "reproducerLength" .= object
              [ "original" .= originalLength
              , "returned" .= length payloadTxs
              ]
          , "truncated" .= truncated
          , "maxJsonBytes" .= conf.maxReproducerJsonBytes
          ]
      (txs, bytesTruncated) = streamableBoundTxsByJsonBytes conf mkPayload shaped
  in mkPayload txs bytesTruncated

streamableArtifactHasReproducer :: Value -> Bool
streamableArtifactHasReproducer = \case
  Object o ->
    case KM.lookup (K.fromText "reproducer") o of
      Just (Object r) ->
        case KM.lookup (K.fromText "best") r of
          Just (Array xs) -> not (null xs)
          _ -> False
      _ -> False
  _ -> False

streamableShapeTxs :: MCPConf -> [Tx] -> [Tx]
streamableShapeTxs conf = streamableShapeTxsLimit conf conf.maxReproducerTxs

streamableShapeTxsLimit :: MCPConf -> Int -> [Tx] -> [Tx]
streamableShapeTxsLimit conf txLimit =
  let trimTxs = if txLimit <= 0 then id else take txLimit
      redact tx = if conf.includeCallData then tx else tx { call = NoCall }
  in map redact . trimTxs

streamableRenderTestId :: EchidnaTest -> Text
streamableRenderTestId test =
  case test.testType of
    PropertyTest name _ -> name
    OptimizationTest name _ -> name
    AssertionTest _ (name, _) _ -> name
    CallTest name _ -> name
    Exploration -> "exploration"

streamableRenderTestType :: TestType -> Text
streamableRenderTestType = \case
  PropertyTest{} -> "property"
  OptimizationTest{} -> "optimization"
  AssertionTest{} -> "assertion"
  CallTest{} -> "call"
  Exploration -> "exploration"

streamableRenderTestState :: TestState -> Text
streamableRenderTestState = \case
  Open -> "open"
  Large _ -> "large"
  Passed -> "passed"
  Unsolvable -> "unsolvable"
  Solved -> "solved"
  Failed _ -> "failed"

streamableShrinkStatus :: TestState -> Text
streamableShrinkStatus = \case
  Open -> "idle"
  Large _ -> "active"
  Failed _ -> "failed"
  _ -> "complete"

streamableFullyShrunk :: TestState -> Bool
streamableFullyShrunk = \case
  Open -> False
  Large _ -> False
  _ -> True

streamableObjectText :: Text -> Value -> Maybe Text
streamableObjectText key = \case
  Object o -> case KM.lookup (K.fromText key) o of
    Just (String t) -> Just t
    _ -> Nothing
  _ -> Nothing

recordStreamableEvent :: MCPConf -> StreamableMCPState -> LocalTime -> Worker.CampaignEvent -> IO ()
recordStreamableEvent conf st ts ev = do
  let (wid, wtype, etype, payload0) =
        case ev of
          Worker.WorkerEvent wid' wtype' e ->
            (Just wid', Just (streamableWorkerTypeText wtype'), streamableWorkerEventType e, streamableWorkerPayload conf e)
          Worker.Failure msg ->
            (Nothing, Nothing, "Failure", toJSON msg)
          Worker.ReproducerSaved f ->
            (Nothing, Nothing, "ReproducerSaved", toJSON f)
          Worker.ServerLog msg ->
            (Nothing, Nothing, "ServerLog", toJSON msg)
  payload <- evaluate (force payload0)
  atomicModifyIORef' st.streamableEventsRef $ \buf ->
    let eventId = buf.streamableNextEventId
        event = StreamableMCPEvent eventId (streamableFormatTimestamp ts) wid wtype etype payload
        items' = buf.streamableEventItems <> [(eventId, event)]
        overflow = length items' - st.streamableMaxEvents
        trimmed = if overflow > 0 then drop overflow items' else items'
    in (StreamableEventBuffer (eventId + 1) trimmed, ())

streamableWorkerTypeText :: Worker.WorkerType -> Text
streamableWorkerTypeText = \case
  Worker.FuzzWorker -> "fuzz"
  Worker.SymbolicWorker -> "symbolic"

streamableWorkerEventType :: Worker.WorkerEvent -> Text
streamableWorkerEventType = \case
  Worker.TestFalsified _ -> "TestFalsified"
  Worker.TestOptimized _ -> "TestOptimized"
  Worker.NewCoverage {} -> "NewCoverage"
  Worker.SymExecError _ -> "SymExecError"
  Worker.SymExecLog _ -> "SymExecLog"
  Worker.Log _ -> "Log"
  Worker.TxSequenceReplayed {} -> "TxSequenceReplayed"
  Worker.TxSequenceReplayFailed {} -> "TxSequenceReplayFailed"
  Worker.WorkerStopped {} -> "WorkerStopped"

streamableWorkerPayload :: MCPConf -> Worker.WorkerEvent -> Value
streamableWorkerPayload conf = \case
  Worker.TestFalsified test -> streamableEventTestPayload conf test
  Worker.TestOptimized test -> streamableEventTestPayload conf test
  Worker.NewCoverage points numCodehashes corpusSize _transactions -> object
    [ "coverage" .= points
    , "contracts" .= numCodehashes
    , "corpus_size" .= corpusSize
    , "transactions" .= ([] :: [Value])
    , "transactionsTruncated" .= True
    ]
  Worker.SymExecError msg -> object ["msg" .= msg]
  Worker.SymExecLog msg -> object ["msg" .= msg]
  Worker.Log msg -> object ["msg" .= msg]
  Worker.TxSequenceReplayed file current total -> object
    [ "file" .= file
    , "current" .= current
    , "total" .= total
    ]
  Worker.TxSequenceReplayFailed file tx -> object
    [ "file" .= file
    , "tx" .= tx
    ]
  Worker.WorkerStopped reason -> object ["reason" .= show reason]

streamableArgsToMap :: Maybe Value -> Map Text Text
streamableArgsToMap = \case
  Just (Object o) -> Map.fromList $ map toPair (KM.toList o)
  _ -> mempty
  where
    toPair (k, v) = (K.toText k, streamableQueryValueText v)

streamableQueryValueText :: Value -> Text
streamableQueryValueText = \case
  String t -> t
  Number n -> case floatingOrInteger n of
    Left (d :: Double) -> T.pack (show d)
    Right (i :: Integer) -> T.pack (show i)
  Bool b -> if b then "true" else "false"
  Null -> "null"
  _ -> ""

streamableReadQueryInt :: Text -> Int -> Map Text Text -> Int
streamableReadQueryInt key def query =
  case Map.lookup key query of
    Nothing -> def
    Just value -> Data.Maybe.fromMaybe def (readMaybe $ T.unpack value)

streamableSplitUri :: Text -> (Text, Map Text Text)
streamableSplitUri uri =
  case T.breakOn "?" uri of
    (path, qs) -> (path, streamableParseQuery $ T.drop 1 qs)

streamableParseQuery :: Text -> Map Text Text
streamableParseQuery qs =
  Map.fromList $ map parsePair $ filter (not . T.null) (T.splitOn "&" qs)
  where
    parsePair p =
      case T.splitOn "=" p of
        (k:v:_) -> (k, v)
        (k:_) -> (k, "")
        _ -> ("", "")

streamableDecodeUriComponent :: Text -> Text
streamableDecodeUriComponent = T.pack . go . T.unpack
  where
    go ('%':a:b:rest)
      | isHexDigit a && isHexDigit b =
          toEnum (digitToInt a * 16 + digitToInt b) : go rest
    go (c:rest) = c : go rest
    go [] = []

streamableFormatTimestamp :: LocalTime -> Text
streamableFormatTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

fromMaybeNull :: Maybe Value -> Value
fromMaybeNull = Data.Maybe.fromMaybe Null

streamableMcpError :: Value -> Int -> Text -> Value
streamableMcpError rid code msg = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= rid
  , "error" .= object ["code" .= code, "message" .= msg]
  ]

streamableMcpResult :: Value -> Value -> Value
streamableMcpResult rid res = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= rid
  , "result" .= res
  ]

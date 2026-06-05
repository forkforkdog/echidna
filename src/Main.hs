{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (unless, when, forM_)
import Control.Monad.Random (getRandomR)
import Control.Monad.Reader (runReaderT, liftIO)
import Data.Aeson (encode)
import Data.Aeson.Key qualified as Aeson.Key
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Function ((&))
import Data.Hashable (hash)
import Data.IORef (readIORef)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Version (showVersion)
import Data.Word (Word8, Word64)
import Main.Utf8 (withUtf8)
import Options.Applicative
import Paths_echidna (version)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, stderr)
import System.IO.CodePage (withCP65001)

import EVM.Dapp (DappInfo(..))
import EVM.Solidity (BuildOutput(..))
import EVM.Types (Addr)

import Echidna
import Echidna.Campaign (isSuccessful)
import Echidna.Config
import Echidna.Onchain qualified as Onchain
import Echidna.Output.Corpus
import Echidna.Output.Foundry
import Echidna.Output.Source
import Echidna.Solidity (compileContracts)
import Echidna.Test (validateTestMode)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Coverage (mergeCoverageMaps)
import Echidna.Types.Solidity
import Echidna.Types.Test (TestMode, EchidnaTest(..), TestConf(..), TestType(..), TestState(..))
import Echidna.UI
import Echidna.Utility (measureIO)

main :: IO ()
main = withUtf8 $ withCP65001 $ do
  opts@Options{..} <- execParser optsParser
  EConfigWithUsage loadedCfg ks _ <-
    maybe (pure (EConfigWithUsage defaultConfig mempty mempty)) parseConfig cliConfigFilepath
  cfg' <- overrideConfig loadedCfg opts
  let cfg = adjustForVerificationMode cfg'

  printProjectName cfg.projectName

  unless cfg.solConf.quiet $
    forM_ ks $ hPutStrLn stderr . ("Warning: unused option: " ++) . Aeson.Key.toString

  buildOutput <- compileContracts cfg.solConf cliFilePath

  -- take the seed from config, otherwise generate a new one
  seed <- maybe (getRandomR (0, maxBound)) pure cfg.campaignConf.seed
  (vm, env, dict) <- prepareContract cfg cliFilePath buildOutput cliSelectedContract seed

  initialCorpus <- loadInitialCorpus env
  -- start ui and run tests
  _campaign <- runReaderT (ui vm dict initialCorpus cliSelectedContract) env

  tests <- traverse readIORef env.testRefs

  checkAssertionsCoverage buildOutput.sources env

  Onchain.saveRpcCache env

  -- save corpus
  case cfg.campaignConf.corpusDir of
    Nothing -> pure ()
    Just dir -> do
      measureIO cfg.solConf.quiet "Saving test reproducers" $
        saveTxs env (dir </> "reproducers") (filter (not . null) $ (.reproducer) <$> tests)

      measureIO cfg.solConf.quiet "Saving corpus" $ do
        corpus <- readIORef env.corpusRef
        saveTxs env (dir </> "coverage") (snd <$> Set.toList corpus)

      let isLargeOrSolved Solved = True
          isLargeOrSolved (Large _) = True
          isLargeOrSolved _ = False
      measureIO cfg.solConf.quiet "Saving foundry reproducers" $ do
        let foundryDir = dir </> "foundry"
            TestConf{testSender} = cfg.testConf
            psender = testSender 0
            saveRepro test = do
              let
                reproducerHash = (show . abs . hash) test.reproducer
                fileName = foundryDir </> "Test." ++ reproducerHash <.> "sol"
                content = foundryTest cliSelectedContract psender test
              liftIO $ writeFile fileName (TL.unpack content)
        liftIO $ createDirectoryIfMissing True foundryDir
        forM_ tests $ \test ->
          case (test.testType, test.state) of
            (AssertionTest{}, state) | isLargeOrSolved state -> saveRepro test
            (PropertyTest{}, state) | isLargeOrSolved state -> saveRepro test
            _ -> pure ()


  -- save coverage reports
  let coverageDir = cfg.campaignConf.coverageDir <|> cfg.campaignConf.corpusDir
  case coverageDir of
    Nothing -> pure ()
    Just dir -> unless (null cfg.campaignConf.coverageFormats) $ measureIO cfg.solConf.quiet "Saving coverage" $ do
      -- We need runId to have a unique directory to save files under so they
      -- don't collide with the next runs. We use the current time for this
      -- as it orders the runs chronologically.
      runId <- fromIntegral . systemSeconds <$> getSystemTime

      when (isJust env.cfg.rpcUrl && not env.cfg.disableOnchainSources) $
        Onchain.saveCoverageReport env runId

      -- save source coverage reports
      let contracts = Map.elems env.dapp.solcByName
      saveCoverages env runId dir buildOutput.sources contracts
      when cfg.campaignConf.coverageLineHits $ do
        covMap <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
        let hits = coverageLineHits buildOutput.sources covMap contracts cfg.campaignConf.coverageExcludes
        LBS.writeFile (dir </> "coverage_hits.json") (encode hits)

  if isSuccessful tests then exitSuccess else exitWith (ExitFailure 1)

data Options = Options
  { cliFilePath         :: NE.NonEmpty FilePath
  , cliWorkers          :: Maybe Word8
  , cliSelectedContract :: Maybe Text
  , cliConfigFilepath   :: Maybe FilePath
  , cliOutputFormat     :: Maybe OutputFormat
  , cliCorpusDir        :: Maybe FilePath
  , cliCoverageDir      :: Maybe FilePath
  , cliCoverageLineHits :: Maybe Bool
  , cliMcpEnabled      :: Maybe Bool
  , cliMcpTransport    :: Maybe MCPTransport
  , cliMcpHost         :: Maybe Text
  , cliMcpPort         :: Maybe Int
  , cliMcpSocket       :: Maybe FilePath
  , cliMcpMaxEvents    :: Maybe Int
  , cliMcpMaxReverts   :: Maybe Int
  , cliMcpMaxTxs       :: Maybe Int
  , cliMcpMaxReproducerArtifacts :: Maybe Int
  , cliMcpReproducerArtifactsLimit :: Maybe Int
  , cliMcpMaxReproducerTxs      :: Maybe Int
  , cliMcpReproducerEventsLimit :: Maybe Int
  , cliMcpReproducerResultTTLMinutes :: Maybe Int
  , cliMcpIncludeCallData :: Maybe Bool
  , cliMcpMaxReproducerJsonBytes :: Maybe Int
  , cliTestMode         :: Maybe TestMode
  , cliAllContracts     :: Bool
  , cliTimeout          :: Maybe Int
  , cliTestLimit        :: Maybe Int
  , cliRpcBlock         :: Maybe Word64
  , cliRpcUrl           :: Maybe Text
  , cliShrinkLimit      :: Maybe Int
  , cliSeqLen           :: Maybe Int
  , cliContractAddr     :: Maybe Addr
  , cliDeployer         :: Maybe Addr
  , cliSender           :: [Addr]
  , cliSeed             :: Maybe Int
  , cliDisableSlither   :: Bool
  , cliCryticArgs       :: Maybe String
  , cliSolcArgs         :: Maybe String
  , cliSymExec          :: Maybe Bool
  , cliSymExecTargets   :: [Text]
  , cliSymExecTimeout   :: Maybe Int
  , cliSymExecNSolvers  :: Maybe Int
  , cliDisableOnchainSources :: Bool
  }

optsParser :: ParserInfo Options
optsParser = info (helper <*> versionOption <*> options) $ fullDesc
  <> progDesc "EVM property-based testing framework"
  <> header "Echidna"

bool :: ReadM Bool
bool = maybeReader (f . map toLower) where
  f "true" = Just True
  f "false" = Just False
  f _ = Nothing

mcpTransport :: ReadM MCPTransport
mcpTransport = eitherReader $ \s ->
  case map toLower s of
    "http" -> Right MCPHttp
    "unix" -> Right MCPUnix
    "stdio" -> Right MCPStdio
    _ -> Left "invalid mcp transport (expected http|unix|stdio)"

options :: Parser Options
options = Options . NE.fromList
  <$> some (argument str (metavar "FILES"
    <> help "Solidity files to analyze"))
  <*> optional (option auto $ long "workers"
    <> metavar "N"
    <> help "Number of workers to run")
  <*> optional (option str $ long "contract"
    <> metavar "CONTRACT"
    <> help "Contract to analyze")
  <*> optional (option str $ long "config"
    <> metavar "CONFIG"
    <> help "Config file (command-line arguments override config options)")
  <*> optional (option auto $ long "format"
    <> metavar "FORMAT"
    <> help "Output format. Either 'json', 'text', 'none'. All these disable interactive UI")
  <*> optional (option str $ long "corpus-dir"
    <> metavar "PATH"
    <> help "Directory to save and load corpus data.")
  <*> optional (option str $ long "coverage-dir"
    <> metavar "PATH"
    <> help "Directory to save coverage reports. Defaults to corpus-dir if not specified.")
  <*> optional (option bool $ long "coverage-line-hits"
    <> metavar "BOOL"
    <> help "Write coverage_hits.json and expose line hits over MCP.")
  <*> optional (option bool $ long "mcp"
    <> metavar "BOOL"
    <> help "Enable MCP server.")
  <*> optional (option mcpTransport $ long "mcp-transport"
    <> metavar "TRANSPORT"
    <> help "MCP transport: http|unix|stdio.")
  <*> optional (option (T.pack <$> str) $ long "mcp-host"
    <> metavar "HOST"
    <> help "MCP host for HTTP transport.")
  <*> optional (option auto $ long "mcp-port"
    <> metavar "PORT"
    <> help "MCP port for HTTP transport.")
  <*> optional (option str $ long "mcp-socket"
    <> metavar "PATH"
    <> help "MCP unix socket path.")
  <*> optional (option auto $ long "mcp-max-events"
    <> metavar "N"
    <> help "MCP ring buffer size for events.")
  <*> optional (option auto $ long "mcp-max-reverts"
    <> metavar "N"
    <> help "MCP ring buffer size for reverts.")
  <*> optional (option auto $ long "mcp-max-txs"
    <> metavar "N"
    <> help "MCP ring buffer size for transactions.")
  <*> optional (option auto $ long "mcp-max-reproducer-artifacts"
    <> metavar "N"
    <> help "MCP maximum reproducer artifacts to retain.")
  <*> optional (option auto $ long "mcp-reproducer-artifacts-limit"
    <> metavar "N"
    <> help "Alias for mcp-max-reproducer-artifacts.")
  <*> optional (option auto $ long "mcp-max-reproducer-txs"
    <> metavar "N"
    <> help "Maximum transactions returned per reproducer.")
  <*> optional (option auto $ long "mcp-reproducer-events-limit"
    <> metavar "N"
    <> help "MCP reproducer event ring buffer size.")
  <*> optional (option auto $ long "mcp-reproducer-result-ttl-minutes"
    <> metavar "N"
    <> help "TTL minutes for MCP reproducer artifacts.")
  <*> optional (option bool $ long "mcp-include-call-data"
    <> metavar "BOOL"
    <> help "Include raw call data in MCP reproducer payloads.")
  <*> optional (option auto $ long "mcp-max-reproducer-json-bytes"
    <> metavar "N"
    <> help "Max JSON bytes for MCP reproducer responses.")
  <*> optional (option str $ long "test-mode"
    <> help "Test mode to use. Either 'property', 'assertion', 'foundry', 'optimization', 'overflow' or 'exploration'" )
  <*> switch (long "all-contracts"
    <> help "Generate calls to all deployed contracts.")
  <*> optional (option auto $ long "timeout"
    <> metavar "INTEGER"
    <> help "Timeout given in seconds.")
  <*> optional (option auto $ long "test-limit"
    <> metavar "INTEGER"
    <> help ("Number of sequences of transactions to generate during testing. Default is " ++ show defaultTestLimit))
  <*> optional (option auto $ long "rpc-block"
    <> metavar "BLOCK"
    <> help "Block number to use when fetching over RPC.")
  <*> optional (option str $ long "rpc-url"
    <> metavar "URL"
    <> help "RPC URL to fetch contracts over.")
  <*> optional (option auto $ long "shrink-limit"
    <> metavar "INTEGER"
    <> help ("Number of tries to attempt to shrink a failing sequence of transactions. Default is " ++ show defaultShrinkLimit))
  <*> optional (option auto $ long "seq-len"
    <> metavar "INTEGER"
    <> help ("Number of transactions to generate during testing. Default is " ++ show defaultSequenceLength))
  <*> optional (option auto $ long "contract-addr"
    <> metavar "ADDRESS"
    <> help ("Address to deploy the contract to test. Default is " ++ show defaultContractAddr))
  <*> optional (option auto $ long "deployer"
    <> metavar "ADDRESS"
    <> help ("Address of the deployer of the contract to test. Default is " ++ show defaultDeployerAddr))
  <*> many (option auto $ long "sender"
    <> metavar "ADDRESS"
    <> help "Addresses to use for the transactions sent during testing. Can be passed multiple times. Check the documentation to see the default values.")
  <*> optional (option auto $ long "seed"
    <> metavar "SEED"
    <> help "Run with a specific seed.")
  <*> switch (long "disable-slither"
    <> help "Disable running Slither.")
  <*> optional (option str $ long "crytic-args"
    <> metavar "ARGS"
    <> help "Additional arguments to use in crytic-compile for the compilation of the contract to test.")
  <*> optional (option str $ long "solc-args"
    <> metavar "ARGS"
    <> help "Additional arguments to use in solc for the compilation of the contract to test.")
  <*> optional (option bool $ long "sym-exec"
    <> metavar "BOOL"
    <> help "Whether to enable the experimental symbolic execution feature.")
  <*> many (option str $ long "sym-exec-target"
    <> metavar "SELECTOR"
    <> help "Target for the symbolic execution run (assuming sym-exec is enabled). Can be passed multiple times. Default is all functions")
  <*> optional (option auto $ long "sym-exec-timeout"
    <> metavar "INTEGER"
    <> help ("Timeout for each symbolic execution run, in seconds (assuming sym-exec is enabled). Default is " ++ show defaultSymExecTimeout))
  <*> optional (option auto $ long "sym-exec-n-solvers"
    <> metavar "INTEGER"
    <> help ("Number of symbolic execution solvers to run in parallel for each task (assuming sym-exec is enabled). Default is " ++ show defaultSymExecNWorkers))
  <*> switch (long "disable-onchain-sources"
    <> help "Disable on-chain coverage reports and fetching of sources from Sourcify and Etherscan.")

versionOption :: Parser (a -> a)
versionOption = infoOption
                  ("Echidna " ++ showVersion version)
                  (long "version" <> help "Show version")

overrideConfig :: EConfig -> Options -> IO EConfig
overrideConfig config Options{..} = do
  envRpcUrl <- Onchain.rpcUrlEnv
  envRpcBlock <- Onchain.rpcBlockEnv
  envEtherscanApiKey <- Onchain.etherscanApiKey
  pure $
    config { solConf = overrideSolConf config.solConf
           , campaignConf = overrideCampaignConf config.campaignConf
           , uiConf = overrideUiConf config.uiConf
           , mcpConf = overrideMcpConf config.mcpConf
           , rpcUrl = cliRpcUrl <|> envRpcUrl <|> config.rpcUrl
           , rpcBlock = cliRpcBlock <|> envRpcBlock <|> config.rpcBlock
           , etherscanApiKey = envEtherscanApiKey <|> config.etherscanApiKey
           , disableOnchainSources = cliDisableOnchainSources || config.disableOnchainSources
           }
           & overrideFormat
  where
    overrideUiConf uiConf = uiConf
      { maxTime = cliTimeout <|> uiConf.maxTime
      }

    overrideFormat cfg =
      case maybe cfg.uiConf.operationMode NonInteractive cliOutputFormat of
        Interactive -> cfg
        NonInteractive Text -> cfg { uiConf = cfg.uiConf { operationMode = NonInteractive Text }}
        nonInteractive -> cfg { uiConf = cfg.uiConf { operationMode = nonInteractive }
                              , solConf = cfg.solConf { quiet = True }
                              }

    overrideCampaignConf campaignConf = campaignConf
      { corpusDir = cliCorpusDir <|> campaignConf.corpusDir
      , coverageDir = cliCoverageDir <|> campaignConf.coverageDir
      , coverageLineHits = fromMaybe campaignConf.coverageLineHits cliCoverageLineHits
      , testLimit = fromMaybe campaignConf.testLimit cliTestLimit
      , shrinkLimit = fromMaybe campaignConf.shrinkLimit cliShrinkLimit
      , seqLen = fromMaybe campaignConf.seqLen cliSeqLen
      , seed = cliSeed <|> campaignConf.seed
      , workers = cliWorkers <|> campaignConf.workers
      , symExec = fromMaybe campaignConf.symExec cliSymExec
      , symExecTargets = if null cliSymExecTargets then campaignConf.symExecTargets else cliSymExecTargets
      , symExecTimeout = fromMaybe campaignConf.symExecTimeout cliSymExecTimeout
      , symExecNSolvers = fromMaybe campaignConf.symExecNSolvers cliSymExecNSolvers
      }

    overrideMcpConf mcpConf = mcpConf
      { enabled = fromMaybe mcpConf.enabled cliMcpEnabled
      , transport = fromMaybe mcpConf.transport cliMcpTransport
      , host = fromMaybe mcpConf.host cliMcpHost
      , port = fromMaybe mcpConf.port cliMcpPort
      , socketPath = fromMaybe mcpConf.socketPath cliMcpSocket
      , maxEvents = fromMaybe mcpConf.maxEvents cliMcpMaxEvents
      , maxReverts = fromMaybe mcpConf.maxReverts cliMcpMaxReverts
      , maxTxs = fromMaybe mcpConf.maxTxs cliMcpMaxTxs
      , maxReproducerArtifacts = fromMaybe
          (fromMaybe mcpConf.maxReproducerArtifacts cliMcpMaxReproducerArtifacts)
          (cliMcpReproducerArtifactsLimit <|> cliMcpMaxReproducerArtifacts)
      , maxReproducerTxs = fromMaybe mcpConf.maxReproducerTxs cliMcpMaxReproducerTxs
      , reproducerEventsLimit = fromMaybe mcpConf.reproducerEventsLimit cliMcpReproducerEventsLimit
      , reproducerResultTTLMinutes = fromMaybe mcpConf.reproducerResultTTLMinutes cliMcpReproducerResultTTLMinutes
      , includeCallData = fromMaybe mcpConf.includeCallData cliMcpIncludeCallData
      , maxReproducerJsonBytes = fromMaybe mcpConf.maxReproducerJsonBytes cliMcpMaxReproducerJsonBytes
      }

    overrideSolConf solConf = solConf
      { disableSlither = cliDisableSlither || solConf.disableSlither
      , solcArgs = fromMaybe solConf.solcArgs cliSolcArgs
      , cryticArgs = maybe solConf.cryticArgs words cliCryticArgs
      , sender = if null cliSender then solConf.sender else Set.fromList cliSender
      , deployer = fromMaybe solConf.deployer cliDeployer
      , contractAddr = fromMaybe solConf.contractAddr cliContractAddr
      , testMode = maybe solConf.testMode validateTestMode cliTestMode
      , allContracts = cliAllContracts || solConf.allContracts
      }

printProjectName :: Maybe Text -> IO ()
printProjectName (Just name) = putStrLn $
    "This is Echidna " <> showVersion version <> " running on project `" <> T.unpack name <> "`"
printProjectName Nothing = pure ()

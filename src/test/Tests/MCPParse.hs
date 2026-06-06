module Tests.MCPParse (mcpParseTests) where

import Control.Exception (ErrorCall, try)
import Data.Aeson (Result(..), Value(..), encode, fromJSON, object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text qualified as T
import Data.Time (LocalTime, UTCTime)
import Data.Vector qualified as Vector
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import EVM.ABI (AbiType(..), AbiValue(..))

import Echidna.MCP
  ( parseArg
  , parseArray
  , parseCall
  , parseFuzzArg
  , parseFuzzCall
  , parseFuzzSequence
  , parsePrimitive
  , splitArgs
  , streamableTestArtifact
  , streamableWorkerPayload
  , streamableArtifactIsInteresting
  , streamableResourcesList
  , streamableDecodeUriComponent
  , streamableEvents
  , streamableStatusSnapshot
  , streamableToolsCall
  , streamableToolsList
  , streamableResourcesRead
  , jsonHeaders
  , StreamableEventBuffer(..)
  , StreamableEventsArgs(..)
  , StreamableMCPEvent(..)
  , StreamableMCPState(..)
  , StreamableRpcRequest(..)
  , recordStreamableEvent
  , streamableStrictRequestBodyWithLimit
  )
import Echidna.Types.Config (MCPConf(..), defaultMCPConf)
import Echidna.Types.Campaign (initialWorkerState)
import Echidna.Types.Test (EchidnaTest(..), TestState(..), TestType(..), TestValue(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxResult(..))
import Echidna.Types.Worker qualified as Worker

uint :: Integer -> AbiValue
uint = AbiUInt 256 . fromInteger

addr :: Integer -> AbiValue
addr = AbiAddress . fromInteger

uintArray :: [Integer] -> AbiValue
uintArray xs =
  AbiArrayDynamic (AbiUIntType 256) (Vector.fromList (map uint xs))

mcpParseTests :: TestTree
mcpParseTests = testGroup "MCP parsing"
  [ primitiveTests
  , arrayTests
  , argTests
  , fuzzArgTests
  , fuzzCallTests
  , fuzzSequenceTests
  , callTests
  , splitArgsTests
  , streamableResourceTests
  , streamablePayloadBoundTests
  , streamableStatusTests
  , streamableReliabilityTests
  ]

primitiveTests :: TestTree
primitiveTests = testGroup "parsePrimitive"
  [ testCase "true"  $ parsePrimitive "true"  @?= Just (AbiBool True)
  , testCase "false" $ parsePrimitive "false" @?= Just (AbiBool False)
  , testCase "True (mixed case)" $ parsePrimitive "True"  @?= Just (AbiBool True)
  , testCase "FALSE (upper case)" $ parsePrimitive "FALSE" @?= Just (AbiBool False)
  , testCase "decimal integer" $ parsePrimitive "42" @?= Just (uint 42)
  , testCase "zero" $ parsePrimitive "0" @?= Just (uint 0)
  , testCase "hex address" $ parsePrimitive "0x1234" @?= Just (addr 0x1234)
  , testCase "trims surrounding whitespace" $
      parsePrimitive "  42  " @?= Just (uint 42)
  , testCase "non-numeric garbage rejected" $
      assertBool "expected Nothing" (isNothing (parsePrimitive "abc"))
  , testCase "empty string rejected" $
      assertBool "expected Nothing" (isNothing (parsePrimitive ""))
  ]

arrayTests :: TestTree
arrayTests = testGroup "parseArray"
  [ testCase "three uints" $
      parseArray "[1,2,3]" @?= Just (uintArray [1, 2, 3])
  , testCase "empty array defaults to uint256 element type" $
      parseArray "[]" @?=
        Just (AbiArrayDynamic (AbiUIntType 256) Vector.empty)
  , testCase "tolerates whitespace around elements" $
      parseArray "[ 1 , 2 ]" @?= Just (uintArray [1, 2])
  , testCase "bool array" $
      parseArray "[true, false]" @?=
        Just (AbiArrayDynamic AbiBoolType
                (Vector.fromList [AbiBool True, AbiBool False]))
  , testCase "rejects mixed element types" $
      assertBool "expected Nothing" (isNothing (parseArray "[1, true]"))
  ]

argTests :: TestTree
argTests = testGroup "parseArg"
  [ testCase "dispatches to array when bracketed" $
      parseArg "[1,2]" @?= Just (uintArray [1, 2])
  , testCase "dispatches to primitive otherwise" $
      parseArg "42" @?= Just (uint 42)
  , testCase "non-numeric garbage rejected" $
      assertBool "expected Nothing" (isNothing (parseArg "abc"))
  ]

fuzzArgTests :: TestTree
fuzzArgTests = testGroup "parseFuzzArg"
  [ testCase "'?' means leave for the fuzzer" $
      parseFuzzArg "?" @?= Just Nothing
  , testCase "'?' surrounded by whitespace still means fuzz" $
      parseFuzzArg "  ?  " @?= Just Nothing
  , testCase "concrete uint wrapped in Just" $
      parseFuzzArg "42" @?= Just (Just (uint 42))
  , testCase "concrete bool wrapped in Just" $
      parseFuzzArg "true" @?= Just (Just (AbiBool True))
  , testCase "non-numeric garbage rejected" $
      assertBool "expected Nothing" (isNothing (parseFuzzArg "abc"))
  ]

fuzzCallTests :: TestTree
fuzzCallTests = testGroup "parseFuzzCall"
  [ testCase "no args" $
      parseFuzzCall "foo()" @?= Just ("foo", [])
  , testCase "two concrete uints" $
      parseFuzzCall "foo(1, 2)" @?=
        Just ("foo", [Just (uint 1), Just (uint 2)])
  , testCase "mix of concrete and fuzz args" $
      parseFuzzCall "transfer(?, 42)" @?=
        Just ("transfer", [Nothing, Just (uint 42)])
  , testCase "array argument alongside fuzz arg" $
      parseFuzzCall "bar([1,2], ?)" @?=
        Just ("bar", [Just (uintArray [1, 2]), Nothing])
  , testCase "missing opening paren rejected" $
      assertBool "expected Nothing" (isNothing (parseFuzzCall "foo)"))
  , testCase "garbage argument fails the whole call" $
      assertBool "expected Nothing" (isNothing (parseFuzzCall "foo(abc)"))
  ]

fuzzSequenceTests :: TestTree
fuzzSequenceTests = testGroup "parseFuzzSequence"
  [ testCase "single call" $
      parseFuzzSequence "foo()" @?= Just [("foo", [])]
  , testCase "two calls separated by ';'" $
      parseFuzzSequence "foo(1); bar(?)" @?=
        Just [ ("foo", [Just (uint 1)])
             , ("bar", [Nothing])
             ]
  , testCase "failure in any call fails the whole sequence" $
      assertBool "expected Nothing"
        (isNothing (parseFuzzSequence "foo(1); bad)"))
  ]

callTests :: TestTree
callTests = testGroup "parseCall"
  [ testCase "no args" $
      parseCall "foo()" @?= Just ("foo", [])
  , testCase "concrete args of mixed primitive types" $
      parseCall "foo(1, true, 0x10)" @?=
        Just ("foo", [uint 1, AbiBool True, addr 0x10])
  , testCase "'?' is rejected (concrete-only parser)" $
      assertBool "expected Nothing" (isNothing (parseCall "foo(?)"))
  , testCase "missing opening paren rejected" $
      assertBool "expected Nothing" (isNothing (parseCall "foo)"))
  ]

splitArgsTests :: TestTree
splitArgsTests = testGroup "splitArgs"
  [ testCase "flat comma-separated list" $
      splitArgs "1,2,3" @?= ["1", "2", "3"]
  , testCase "respects single-level bracket nesting" $
      splitArgs "[1,2],3" @?= ["[1,2]", "3"]
  , testCase "respects deep bracket nesting" $
      splitArgs "[[1,2],3],4" @?= ["[[1,2],3]", "4"]
  , testCase "single token (no commas)" $
      splitArgs "a" @?= ["a"]
  ]

streamableResourceTests :: TestTree
streamableResourceTests = testGroup "streamableResourcesList"
  [ testCase "advertises single reproducer snapshots" $
      assertBool "missing single reproducer URI template" $
        T.pack "echidna://run/reproducer/<test-key>" `elem` resourceUris streamableResourcesList
  , testCase "decodes encoded reproducer resource keys" $
      streamableDecodeUriComponent "test%3A0%3Aassertion" @?= "test:0:assertion"
  ]

streamablePayloadBoundTests :: TestTree
streamablePayloadBoundTests = testGroup "streamable payload bounds"
  [ testCase "snapshot trims reproducer transaction count" $ do
      let conf = defaultMCPConf { maxReproducerTxs = 3, maxReproducerJsonBytes = 0, includeCallData = True }
          artifact = streamableTestArtifact conf 0 (mkStreamableTest 10)
      objectArrayLength ["reproducer", "best"] artifact @?= Just 3
      objectBool ["reproducer", "truncated"] artifact @?= Just True
  , testCase "event payload uses event reproducer limit" $ do
      let conf = defaultMCPConf { reproducerEventsLimit = 2, maxReproducerJsonBytes = 0, includeCallData = True }
          payload = streamableWorkerPayload conf (Worker.TestFalsified (mkStreamableTest 10))
      objectArrayLength ["reproducer"] payload @?= Just 2
      objectBool ["truncated"] payload @?= Just True
  , testCase "snapshot respects JSON byte budget" $ do
      let test = mkStreamableTest 10
          loose = defaultMCPConf { maxReproducerTxs = 10, maxReproducerJsonBytes = 0, includeCallData = True }
          bounded = defaultMCPConf { maxReproducerTxs = 10, maxReproducerJsonBytes = 1200, includeCallData = True }
          looseBytes = LBS.length . encode $ streamableTestArtifact loose 0 test
          artifact = streamableTestArtifact bounded 0 test
          boundedBytes = LBS.length (encode artifact)
      assertBool "expected bounded payload to be smaller" (boundedBytes < looseBytes)
      assertBool "expected bounded payload to fit configured budget" (boundedBytes <= 1200)
      objectBool ["reproducer", "truncated"] artifact @?= Just True
  ]

streamableStatusTests :: TestTree
streamableStatusTests = testGroup "streamable status"
  [ testCase "keeps counters coherent when failures are visible before worker callback" $ do
      let now = read "2026-06-06 00:00:00 UTC" :: UTCTime
          payload = streamableStatusSnapshot now now [initialWorkerState] 15 15 233468 4 2
      objectInt ["runs"] payload @?= Just 15
      objectInt ["counters", "totalCalls"] payload @?= Just 15
      objectInt ["counters", "successCalls"] payload @?= Just 0
      objectInt ["counters", "failedCalls"] payload @?= Just 15
      objectInt ["coveragePoints"] payload @?= Just 233468
  ]

streamableReliabilityTests :: TestTree
streamableReliabilityTests = testGroup "streamable MCP reliability"
  [ testCase "response headers do not advertise a fake session id" $
      length jsonHeaders @?= 1
  , testCase "tool schemas advertise accepted arguments" $ do
      assertBool "get_events.limit schema missing" $
        toolSchemaHasProperty "get_events" "limit" streamableToolsList
      assertBool "get_reproducer.testKey schema missing" $
        toolSchemaHasProperty "get_reproducer" "testKey" streamableToolsList
  , testCase "JSON-RPC parser requires version 2.0" $
      case fromJSON (object ["jsonrpc" .= ("1.0" :: T.Text), "method" .= ("ping" :: T.Text)]) :: Result StreamableRpcRequest of
        Error _ -> pure ()
        Success _ -> assertFailure "expected invalid jsonrpc version to fail parsing"
  , testCase "unknown tools return JSON-RPC invalid params" $ do
      st <- mkStreamableState 10
      payload <- streamableToolsCall Null undefined [] st $
        Just (object ["name" .= ("does_not_exist" :: T.Text), "arguments" .= object []])
      objectInt ["error", "code"] payload @?= Just (-32602)
  , testCase "invalid known-tool arguments return an MCP tool error result" $ do
      st <- mkStreamableState 10
      payload <- streamableToolsCall Null undefined [] st $
        Just (object ["name" .= ("get_events" :: T.Text), "arguments" .= object ["limit" .= (0 :: Int)]])
      objectBool ["result", "isError"] payload @?= Just True
  , testCase "unknown resources return JSON-RPC invalid params" $ do
      st <- mkStreamableState 10
      payload <- streamableResourcesRead Null undefined [] st $
        Just (object ["uri" .= ("echidna://bad/resource" :: T.Text)])
      objectInt ["error", "code"] payload @?= Just (-32602)
  , testCase "request body reader accepts chunks within the configured limit" $ do
      chunks <- newIORef [BS.replicate 3 65, BS.replicate 2 66, BS.empty]
      result <- streamableStrictRequestBodyWithLimit 5 (nextBodyChunk chunks)
      case result of
        Right body -> do
          LBS.length body @?= 5
          readIORef chunks >>= (@?= [])
        Left bytes -> assertFailure ("unexpected body rejection at " <> show bytes <> " bytes")
  , testCase "request body reader rejects as soon as the configured limit is crossed" $ do
      chunks <- newIORef [BS.replicate 4 65, BS.replicate 4 66, error "reader consumed after rejection"]
      result <- streamableStrictRequestBodyWithLimit 5 (nextBodyChunk chunks)
      result @?= Left 8
  , testCase "event history is trimmed when events are recorded" $ do
      st <- mkStreamableState 2
      let ts = read "2026-06-06 00:00:00" :: LocalTime
      mapM_ (\i -> recordStreamableEvent defaultMCPConf st ts (Worker.WorkerEvent i Worker.FuzzWorker (Worker.Log ("event-" <> show i)))) [0..2]
      StreamableEventBuffer nextId items <- readIORef st.streamableEventsRef
      nextId @?= 3
      map fst items @?= [1, 2]
  , testCase "get_events without since returns latest retained events in chronological order" $ do
      st <- mkStreamableState 10
      let ts = read "2026-06-06 00:00:00" :: LocalTime
      mapM_ (\i -> recordStreamableEvent defaultMCPConf st ts (Worker.WorkerEvent i Worker.FuzzWorker (Worker.Log ("event-" <> show i)))) [0..4]
      payload <- streamableEvents st (StreamableEventsArgs Nothing (Just 2))
      eventIds payload @?= [3, 4]
  , testCase "get_events with since returns first events after cursor" $ do
      st <- mkStreamableState 10
      let ts = read "2026-06-06 00:00:00" :: LocalTime
      mapM_ (\i -> recordStreamableEvent defaultMCPConf st ts (Worker.WorkerEvent i Worker.FuzzWorker (Worker.Log ("event-" <> show i)))) [0..4]
      payload <- streamableEvents st (StreamableEventsArgs (Just 1) (Just 2))
      eventIds payload @?= [2, 3]
  , testCase "zero-transaction failing artifacts remain visible" $ do
      let artifact = streamableTestArtifact defaultMCPConf 0 (mkStreamableTest 0)
      assertBool "expected large zero-transaction artifact to be interesting" $
        streamableArtifactIsInteresting artifact
      objectBool ["reproducer", "requiresTransactions"] artifact @?= Just False
  , testCase "reproducer metadata marks redacted calldata" $ do
      let artifact = streamableTestArtifact defaultMCPConf 0 (mkStreamableTest 1)
      objectBool ["reproducer", "redacted"] artifact @?= Just True
      objectText ["reproducer", "callDataPolicy"] artifact @?= Just "redacted"
  , testCase "pure reproducer snapshots are unverified until replayed" $ do
      let artifact = streamableTestArtifact defaultMCPConf 0 (mkStreamableTestWithState (Large 0) 1)
      objectText ["reproducer", "trust", "source"] artifact @?= Just "current-test-ref"
      objectText ["reproducer", "trust", "status"] artifact @?= Just "unverified"
      objectBool ["reproducer", "trust", "verified"] artifact @?= Just False
      objectText ["reproducer", "trust", "stability"] artifact @?= Just "shrinking"
  , testCase "solved pure snapshots are final but still unverified until replayed" $ do
      let artifact = streamableTestArtifact defaultMCPConf 0 (mkStreamableTestWithState Solved 1)
      objectText ["reproducer", "trust", "status"] artifact @?= Just "unverified"
      objectBool ["reproducer", "trust", "verified"] artifact @?= Just False
      objectText ["reproducer", "trust", "stability"] artifact @?= Just "final"
      objectText ["reproducer", "trust", "verifiedInvariant"] artifact @?= Just "same-test-same-sequence-falsified"
  , testCase "pure event reproducers are unverified until replayed" $ do
      let payload = streamableWorkerPayload defaultMCPConf (Worker.TestFalsified (mkStreamableTest 1))
      objectText ["reproducerTrust", "source"] payload @?= Just "event-payload"
      objectText ["reproducerTrust", "status"] payload @?= Just "unverified"
      objectBool ["reproducerTrust", "verified"] payload @?= Just False
      objectText ["reproducerTrust", "sourceOfTruth"] payload @?= Nothing
  , testCase "event payloads are forced before insertion" $ do
      st <- mkStreamableState 10
      let ts = read "2026-06-06 00:00:00" :: LocalTime
          event = Worker.WorkerEvent 0 Worker.FuzzWorker (Worker.Log (error "unforced event payload"))
      result <- try (recordStreamableEvent defaultMCPConf st ts event) :: IO (Either ErrorCall ())
      case result of
        Left _ -> pure ()
        Right _ -> assertFailure "expected recordStreamableEvent to force and reject the bad payload"
      StreamableEventBuffer _ items <- readIORef st.streamableEventsRef
      length items @?= 0
  , testCase "new coverage events do not retain transaction arrays" $ do
      st <- mkStreamableState 10
      let ts = read "2026-06-06 00:00:00" :: LocalTime
          event = Worker.WorkerEvent 0 Worker.FuzzWorker (Worker.NewCoverage 7 2 3 (error "transactions should not be retained"))
      recordStreamableEvent defaultMCPConf st ts event
      StreamableEventBuffer _ items <- readIORef st.streamableEventsRef
      case items of
        [(_, ev)] -> do
          objectInt ["coverage"] ev.streamablePayload @?= Just 7
          objectArrayLength ["transactions"] ev.streamablePayload @?= Just 0
          objectBool ["transactionsTruncated"] ev.streamablePayload @?= Just True
        _ -> assertFailure ("expected one retained event, got " <> show (length items))
  ]

mkStreamableTest :: Int -> EchidnaTest
mkStreamableTest = mkStreamableTestWithState (Large 0)

mkStreamableTestWithState :: TestState -> Int -> EchidnaTest
mkStreamableTestWithState testState txCount = EchidnaTest
  { state = testState
  , testType = PropertyTest "echidna_prop" 0
  , value = NoValue
  , reproducer = replicate txCount sampleTx
  , result = ReturnFalse
  , vm = Nothing
  , workerId = Just 1
  }

sampleTx :: Tx
sampleTx = Tx (SolCall ("foo", [uint 1])) 1 2 3 4 5 (6, 7)

resourceUris :: Value -> [T.Text]
resourceUris (Object o) =
  case KM.lookup (K.fromText $ T.pack "resources") o of
    Just (Array xs) -> mapMaybe resourceUri (Vector.toList xs)
    _ -> []
resourceUris _ = []

resourceUri :: Value -> Maybe T.Text
resourceUri (Object o) =
  case KM.lookup (K.fromText $ T.pack "uri") o of
    Just (String uri) -> Just uri
    _ -> Nothing
resourceUri _ = Nothing

objectArrayLength :: [T.Text] -> Value -> Maybe Int
objectArrayLength [] (Array xs) = Just (Vector.length xs)
objectArrayLength (key:keys) (Object o) =
  KM.lookup (K.fromText key) o >>= objectArrayLength keys
objectArrayLength _ _ = Nothing

objectBool :: [T.Text] -> Value -> Maybe Bool
objectBool [] (Bool b) = Just b
objectBool (key:keys) (Object o) =
  KM.lookup (K.fromText key) o >>= objectBool keys
objectBool _ _ = Nothing

objectInt :: [T.Text] -> Value -> Maybe Int
objectInt [] (Number n) =
  let decoded :: Result Int
      decoded = fromJSON (Number n)
  in case decoded of
       Success i -> Just i
       Error _ -> Nothing
objectInt (key:keys) (Object o) =
  KM.lookup (K.fromText key) o >>= objectInt keys
objectInt _ _ = Nothing

eventIds :: Value -> [Int]
eventIds (Object o) =
  case KM.lookup (K.fromText $ T.pack "events") o of
    Just (Array xs) -> mapMaybe (objectInt ["id"]) (Vector.toList xs)
    _ -> []
eventIds _ = []

toolSchemaHasProperty :: T.Text -> T.Text -> Value -> Bool
toolSchemaHasProperty toolName propName (Object o) =
  case KM.lookup (K.fromText $ T.pack "tools") o of
    Just (Array xs) -> any hasProp (Vector.toList xs)
    _ -> False
  where
    hasProp (Object tool) =
      case KM.lookup (K.fromText $ T.pack "name") tool of
        Just (String name) | name == toolName ->
          case KM.lookup (K.fromText $ T.pack "inputSchema") tool of
            Just (Object schema) ->
              case KM.lookup (K.fromText $ T.pack "properties") schema of
                Just (Object props) -> KM.member (K.fromText propName) props
                _ -> False
            _ -> False
        _ -> False
    hasProp _ = False
toolSchemaHasProperty _ _ _ = False

objectText :: [T.Text] -> Value -> Maybe T.Text
objectText [] (String t) = Just t
objectText (key:keys) (Object o) =
  KM.lookup (K.fromText key) o >>= objectText keys
objectText _ _ = Nothing

mkStreamableState :: Int -> IO StreamableMCPState
mkStreamableState maxEvents = do
  eventsRef <- newIORef (StreamableEventBuffer 0 [])
  coverageRef <- newIORef (Object mempty)
  refreshingRef <- newIORef False
  let started = read "2026-06-06 00:00:00 UTC" :: UTCTime
  pure $ StreamableMCPState eventsRef started maxEvents 65536 coverageRef refreshingRef Nothing

nextBodyChunk :: IORef [BS.ByteString] -> IO BS.ByteString
nextBodyChunk ref =
  atomicModifyIORef' ref $ \case
    [] -> ([], BS.empty)
    chunk:rest -> (rest, chunk)

module Tests.MCPParse (mcpParseTests) where

import Data.Aeson (Value(..), encode)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isNothing, mapMaybe)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

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
  , streamableResourcesList
  )
import Echidna.Types.Config (MCPConf(..), defaultMCPConf)
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

mkStreamableTest :: Int -> EchidnaTest
mkStreamableTest txCount = EchidnaTest
  { state = Large 0
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

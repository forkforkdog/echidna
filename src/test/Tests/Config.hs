module Tests.Config (configTests) where

import Control.Monad (void)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.Maybe (isJust, isNothing)
import Data.Yaml qualified as Y
import Optics.Core (sans)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)

import Echidna.Config (defaultConfig, parseConfig)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (EConfigWithUsage(..), EConfig(..), MCPConf(..), MCPTransport(..), defaultMCPConf, validateMCPConf)
import Echidna.Types.Tx (TxConf(..))

configTests :: TestTree
configTests = testGroup "Configuration tests" $
  [ testCase file . void $ parseConfig file | file <- files ] ++
  [ testCase "parse \"coverage: true\"" $ do
      config <- (.econfig) <$> parseConfig "coverage/test.yaml"
      assertBool "" $ isJust config.campaignConf.knownCoverage
  , testCase "coverage enabled by default" $
      assertBool "" $ isJust defaultConfig.campaignConf.knownCoverage
  , testCase "parse corpusDir" $ do
      config <- (.econfig) <$> parseConfig "research/bran_bar.yaml"
      assertBool "" $ config.campaignConf.corpusDir == Just "corpus"
  , testCase "parse coverageDir" $ do
      config <- (.econfig) <$> parseConfig "basic/coverage-test.yaml"
      assertBool "" $ config.campaignConf.coverageDir == Just "coverage-reports"
  , testCase "corpusDir and coverageDir independent" $ do
      config <- (.econfig) <$> parseConfig "basic/coverage-test.yaml"
      assertBool "corpusDir should be set" $ config.campaignConf.corpusDir == Just "corpus-data"
      assertBool "coverageDir should be set" $ config.campaignConf.coverageDir == Just "coverage-reports"
  , testCase "coverageDir fallback to corpusDir" $ do
      config <- (.econfig) <$> parseConfig "basic/corpus-fallback-test.yaml"
      assertBool "corpusDir should be set" $ config.campaignConf.corpusDir == Just "test-corpus"
      assertBool "coverageDir should not be set" $ isNothing (config.campaignConf.coverageDir)
  , testCase "corpusDir defaults to Nothing" $
      assertBool "" $ isNothing (defaultConfig.campaignConf.corpusDir)
  , testCase "coverageDir defaults to Nothing" $
      assertBool "" $ isNothing (defaultConfig.campaignConf.coverageDir)
  , testCase "parse mcp bounds" $ do
      let yaml = BS8.pack $ unlines
            [ "mcp:"
            , "  enabled: true"
            , "  maxEvents: 11"
            , "  maxReverts: 12"
            , "  maxTxs: 13"
            , "  maxReproducerArtifacts: 14"
            , "  maxReproducerTxs: 15"
            , "  reproducerEventsLimit: 16"
            , "  reproducerResultTTLMinutes: 17"
            , "  includeCallData: true"
            , "  maxReproducerJsonBytes: 18"
            , "  maxRequestBytes: 19"
            ]
      case Y.decodeEither' yaml of
        Right (c :: EConfigWithUsage) -> do
          let mcpConf = c.econfig.mcpConf
          assertBool "mcp should be enabled" mcpConf.enabled
          assertEqual "maxEvents" 11 mcpConf.maxEvents
          assertEqual "maxReverts" 12 mcpConf.maxReverts
          assertEqual "maxTxs" 13 mcpConf.maxTxs
          assertEqual "maxReproducerArtifacts" 14 mcpConf.maxReproducerArtifacts
          assertEqual "maxReproducerTxs" 15 mcpConf.maxReproducerTxs
          assertEqual "reproducerEventsLimit" 16 mcpConf.reproducerEventsLimit
          assertEqual "reproducerResultTTLMinutes" 17 mcpConf.reproducerResultTTLMinutes
          assertBool "includeCallData should be enabled" mcpConf.includeCallData
          assertEqual "maxReproducerJsonBytes" 18 mcpConf.maxReproducerJsonBytes
          assertEqual "maxRequestBytes" 19 mcpConf.maxRequestBytes
        Left e -> assertFailure $ "unexpected decoding error: " <> show e
  , testCase "parse mcp maxReproducers alias" $ do
      let yaml = BS8.pack $ unlines
            [ "mcp:"
            , "  maxReproducers: 21"
            ]
      case Y.decodeEither' yaml of
        Right (c :: EConfigWithUsage) ->
          assertEqual "maxReproducers" 21 c.econfig.mcpConf.maxReproducerArtifacts
        Left e -> assertFailure $ "unexpected decoding error: " <> show e
  , testCase "reject enabled unsupported mcp transports" $ do
      case validateMCPConf defaultMCPConf { enabled = True, transport = MCPUnix } of
        Left _ -> pure ()
        Right _ -> assertFailure "expected enabled mcp.transport=unix to be rejected"
      case validateMCPConf defaultMCPConf { enabled = True, transport = MCPStdio } of
        Left _ -> pure ()
        Right _ -> assertFailure "expected enabled mcp.transport=stdio to be rejected"
  , testCase "allow unsupported mcp transport while mcp is disabled" $
      case validateMCPConf defaultMCPConf { enabled = False, transport = MCPUnix } of
        Right _ -> pure ()
        Left e -> assertFailure $ "unexpected validation error: " <> e
  , testCase "default.yaml" $ do
      EConfigWithUsage _ bad unset <- parseConfig "basic/default.yaml"
      assertBool ("unused options: " ++ show bad) $ null bad
      let unset' = unset & sans "seed"
      assertBool ("unset options: " ++ show unset') $ null unset'
  , testCase "W256 decoding" $ do
      let maxW256  = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
          overW256 = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0"
      case Y.decodeEither' ("maxGasprice: " <> maxW256) of
        Right (c :: EConfigWithUsage) | c.econfig.txConf.maxGasprice == maxBound -> pure ()
        Right _ -> assertFailure "wrong value decoded"
        Left e -> assertFailure $ "unexpected decoding error: " <> show e
      case Y.decodeEither' ("maxGasprice: " <> overW256) of
        Right (_ :: EConfigWithUsage) -> assertFailure "should not decode"
        Left _ -> pure ()
  ]
  where files = ["basic/config.yaml", "basic/default.yaml", "basic/coverage-test.yaml", "basic/corpus-fallback-test.yaml"]

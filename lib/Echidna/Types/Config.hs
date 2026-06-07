module Echidna.Types.Config where

import Control.Concurrent (Chan)
import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Key (Key)
import Data.IORef (IORef)
import Data.Set (Set)
import Echidna.Types.InterWorker (Bus)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime)
import Data.Word (Word64)

import EVM.Dapp (DappInfo)
import EVM.Fetch qualified as Fetch
import EVM.Solidity (SourceCache)
import EVM.Types (Addr, W256)

import Echidna.SourceAnalysis.Slither (SlitherInfo)
import Echidna.SourceMapping (CodehashMap)
import Echidna.Types.Cache
import Echidna.Types.Campaign (CampaignConf)
import Echidna.Types.Corpus (Corpus)
import Echidna.Types.Coverage (CoverageMap)
import Echidna.Types.Solidity (SolConf)
import Echidna.Types.Test (TestConf, EchidnaTest)
import Echidna.Types.Tx (TxConf)
import Echidna.Types.Worker (CampaignEvent)
import Echidna.Types.World (World)

data MCPTransport = MCPHttp | MCPUnix | MCPStdio deriving (Show, Eq)

instance FromJSON MCPTransport where
  parseJSON = withText "MCPTransport" $ \t ->
    case T.toLower t of
      "http" -> pure MCPHttp
      "unix" -> pure MCPUnix
      "stdio" -> pure MCPStdio
      _ -> fail "invalid mcp.transport (expected http|unix|stdio)"

data MCPConf = MCPConf
  { enabled    :: Bool
  , transport  :: MCPTransport
  , host       :: Text
  , port       :: Int
  , socketPath :: FilePath
  , maxEvents  :: Int
  , maxReverts :: Int
  , maxTxs     :: Int
  , maxReproducerArtifacts :: Int
  , maxReproducerTxs      :: Int
  , reproducerEventsLimit  :: Int
  , reproducerResultTTLMinutes :: Int
  , includeCallData        :: Bool
  , maxReproducerJsonBytes :: Int
  , maxRequestBytes        :: Int
  } deriving (Show, Eq)

validateMCPConf :: MCPConf -> Either String MCPConf
validateMCPConf conf
  | not conf.enabled = Right conf
  | conf.transport == MCPHttp = Right conf
  | otherwise = Left $ "mcp.transport=" <> show conf.transport <> " is parsed but not implemented"

defaultMCPConf :: MCPConf
defaultMCPConf = MCPConf
  { enabled = False
  , transport = MCPHttp
  , host = "127.0.0.1"
  , port = 9001
  , socketPath = "/tmp/echidna.mcp.sock"
  , maxEvents = 5000
  , maxReverts = 1000
  , maxTxs = 1000
  , maxReproducerArtifacts = 5000
  , maxReproducerTxs = 128
  , reproducerEventsLimit = 500
  , reproducerResultTTLMinutes = 120
  , includeCallData = False
  , maxReproducerJsonBytes = 256000
  , maxRequestBytes = 65536
  }

data OperationMode = Interactive | NonInteractive OutputFormat deriving (Show, Eq)
data OutputFormat = Text | JSON | None deriving (Show, Eq)
data UIConf = UIConf { maxTime       :: Maybe Int
                     , operationMode :: OperationMode
                     }

-- | An address involved with a 'Transaction' is either the sender, the recipient, or neither of those things.
data Role = Sender | Receiver

-- | Rules for pretty-printing addresses based on their role in a transaction.
type Names = Role -> Addr -> String

-- | Our big glorious global config type, just a product of each local config.,
data EConfig = EConfig
  { campaignConf :: CampaignConf
  , namesConf :: Names
  , solConf :: SolConf
  , testConf :: TestConf
  , txConf :: TxConf
  , uiConf :: UIConf
  , mcpConf :: MCPConf

  , allEvents :: Bool
  , rpcUrl :: Maybe Text
  , rpcBlock :: Maybe Word64
  , etherscanApiKey :: Maybe Text
  , projectName :: Maybe Text
  , disableOnchainSources :: Bool
  }

instance Read OutputFormat where
  readsPrec _ =
    \case 't':'e':'x':'t':r -> [(Text, r)]
          'j':'s':'o':'n':r -> [(JSON, r)]
          'n':'o':'n':'e':r -> [(None, r)]
          _ -> []


data EConfigWithUsage = EConfigWithUsage
  { econfig   :: EConfig
  , badkeys   :: Set Key
  , unsetkeys :: Set Key
  }

data Env = Env
  { cfg :: EConfig
  , dapp :: DappInfo
  , sourceCache :: SourceCache

  -- | Whether stdout supports ANSI escape codes. Detected once at startup;
  -- false when output is redirected to a file or pipe so traces can be
  -- saved without unrenderable color codes.
  , useColor :: Bool

  -- | Shared between all workers. Events are fairly rare so contention is
  -- minimal.
  , eventQueue :: Chan (LocalTime, CampaignEvent)
  , bus :: Bus

  , testRefs :: [IORef EchidnaTest]
  , coverageRefInit :: IORef CoverageMap
  , coverageRefRuntime :: IORef CoverageMap
  , corpusRef :: IORef Corpus

  , slitherInfo :: Maybe SlitherInfo
  , codehashMap :: CodehashMap
  , fetchSession :: Fetch.Session
  , contractNameCache :: IORef ContractNameCache
  , chainId :: Maybe W256
  , world :: World
  }

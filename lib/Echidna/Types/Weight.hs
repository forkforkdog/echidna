module Echidna.Types.Weight where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

-- | Configuration for weighted function selection during fuzzing
data WeightConfig = WeightConfig
  { functionWeights :: Map Text Double  -- ^ Function signature -> weight mapping
  , defaultWeight   :: Double           -- ^ Default weight for unlisted functions
  } deriving (Show, Eq)

-- | Default weight configuration (uniform selection)
defaultWeightConfig :: WeightConfig
defaultWeightConfig = WeightConfig
  { functionWeights = Map.empty
  , defaultWeight   = 1.0
  }

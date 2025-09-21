module Echidna.Output.PeriodicSave where

import Control.Concurrent (ThreadId, threadDelay, forkIO, killThread)
import Control.Monad (forever, when)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Text (Text)
import Data.Text.IO (writeFile)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prelude hiding (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import Text.Printf (printf)

import EVM.Dapp (DappInfo(..))
import EVM.Solidity (SourceCache(..), SolcContract)

import Echidna.Output.Source
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Coverage (CoverageFileType(..), mergeCoverageMaps, FrozenCoverageMap)
import Echidna.Types.Solidity (SolConf(..))

-- | Spawn a thread that periodically saves coverage data during campaign execution
-- Returns the ThreadId so it can be killed when the campaign ends
spawnPeriodicSaver
  :: Env
  -> Int          -- ^ seed for filename
  -> FilePath     -- ^ base directory to save to
  -> SourceCache  -- ^ source cache
  -> [SolcContract] -- ^ contracts
  -> IO (Maybe ThreadId)
spawnPeriodicSaver env seed dir sources contracts =
  case env.cfg.campaignConf.saveEvery of
    Nothing -> pure Nothing
    Just minutes -> do
      let intervalMicroseconds = minutes * 60 * 1000000  -- Convert minutes to microseconds
      tid <- forkIO $ forever $ do
        -- Wait for the specified interval
        threadDelay intervalMicroseconds

        -- Get current timestamp for unique filename
        timestamp <- round <$> getPOSIXTime

        -- Save the current coverage snapshot
        saveCoverageSnapshot env seed timestamp dir sources contracts

      pure (Just tid)

-- | Save a snapshot of the current coverage data with a timestamp
saveCoverageSnapshot
  :: Env
  -> Int          -- ^ seed
  -> Int          -- ^ timestamp
  -> FilePath     -- ^ base directory
  -> SourceCache  -- ^ source cache
  -> [SolcContract] -- ^ contracts
  -> IO ()
saveCoverageSnapshot env seed timestamp dir sources contracts = do
  -- Create the snapshots subdirectory if it doesn't exist
  let snapshotDir = dir </> "coverage-snapshots"
  createDirectoryIfMissing True snapshotDir

  -- Merge the current coverage maps (thread-safe since we're only reading)
  coverage <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime

  -- Save each format with timestamp in filename
  let fileTypes = env.cfg.campaignConf.coverageFormats
  mapM_ (\ty -> saveCoverageWithTimestamp ty seed timestamp snapshotDir sources contracts coverage) fileTypes

  -- Log that we saved a snapshot (if not in quiet mode)
  when (not env.cfg.solConf.quiet) $
    putStrLn $ printf "Coverage snapshot saved at %d" timestamp

-- | Save coverage with timestamp in filename
saveCoverageWithTimestamp
  :: CoverageFileType
  -> Int          -- ^ seed
  -> Int          -- ^ timestamp
  -> FilePath     -- ^ directory
  -> SourceCache  -- ^ source cache
  -> [SolcContract] -- ^ contracts
  -> FrozenCoverageMap
  -> IO ()
saveCoverageWithTimestamp fileType seed timestamp dir sc cs covMap = do
  let extension = coverageFileExtension fileType
      -- Include both seed and timestamp in filename for uniqueness
      fn = dir </> printf "covered.%d.%d%s" seed timestamp extension
      -- Reuse existing pretty-printing logic from Output.Source
      cc = ppCoveredCode fileType sc cs covMap
  -- Write the coverage report to file
  createDirectoryIfMissing True dir
  writeFile fn cc
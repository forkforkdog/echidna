{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}

module Echidna.Output.Source where

import Prelude hiding (writeFile)

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.IORef (readIORef)
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.TLS.GHC (allTLS)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import HTMLEntities.Text qualified as HTML
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)

import EVM.Dapp (srcMapCodePos, DappInfo(..))
import EVM.Solidity (SourceCache(..), SrcMap, SolcContract(..))

import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Coverage (OpIx, unpackTxResults, FrozenCoverageMap, CoverageFileType (..), mergeCoverageMaps, StatsInfo, FrozenStatsMap, freezeStatsMap, mergeFrozenStatsMaps)
import Echidna.Types.Tx (TxResult(..))
import Echidna.SourceAnalysis.Slither (AssertLocation(..), assertLocationList, SlitherInfo(..))

saveCoverages
  :: Env
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> IO ()
saveCoverages env seed d sc cs = do
  let fileTypes = env.cfg.campaignConf.coverageFormats
  coverage <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
  -- Combine stats from all threads if tracking was enabled
  maybeStats <- combineStats env
  mapM_ (\ty -> saveCoverage ty seed d sc cs coverage maybeStats) fileTypes

-- | Combine stats from all threads into a single FrozenStatsMap
combineStats :: Env -> IO (Maybe FrozenStatsMap)
combineStats env = case (env.statsRefInit, env.statsRefRuntime) of
  (Just initTLS, Just runtimeTLS) -> do
    -- Get all thread-local stats maps
    initMaps <- allTLS initTLS >>= mapM readIORef
    runtimeMaps <- allTLS runtimeTLS >>= mapM readIORef
    -- Freeze and combine init maps
    frozenInitMaps <- mapM freezeStatsMap initMaps
    let combinedInit = foldl' mergeFrozenStatsMaps Map.empty frozenInitMaps
    -- Freeze and combine runtime maps
    frozenRuntimeMaps <- mapM freezeStatsMap runtimeMaps
    let combinedRuntime = foldl' mergeFrozenStatsMaps Map.empty frozenRuntimeMaps
    -- Merge init and runtime (runtime takes precedence for same codehash)
    pure $ Just $ mergeFrozenStatsMaps combinedInit combinedRuntime
  _ -> pure Nothing

saveCoverage
  :: CoverageFileType
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> FrozenCoverageMap
  -> Maybe FrozenStatsMap
  -> IO ()
saveCoverage fileType seed d sc cs covMap maybeStats = do
  let extension = coverageFileExtension fileType
      fn = d </> "covered." <> show seed <> extension
      cc = ppCoveredCode fileType sc cs covMap maybeStats
  createDirectoryIfMissing True d
  writeFile fn cc

coverageFileExtension :: CoverageFileType -> String
coverageFileExtension Lcov = ".lcov"
coverageFileExtension Html = ".html"
coverageFileExtension Txt = ".txt"

-- | Pretty-print the covered code
ppCoveredCode :: CoverageFileType -> SourceCache -> [SolcContract] -> FrozenCoverageMap -> Maybe FrozenStatsMap -> Text
ppCoveredCode fileType sc cs s maybeStats | null s = "Coverage map is empty"
  | otherwise =
  let
    -- List of covered lines during the fuzzing campaign
    covLines = srcMapCov sc s maybeStats cs
    -- Collect all the possible lines from all the files
    allFiles = (\(path, src) -> (path, V.fromList (decodeUtf8 <$> BS.split 0xa src))) <$> Map.elems sc.files
    -- Excludes lines such as comments or blanks
    runtimeLinesMap = buildRuntimeLinesMap sc cs
    hasStats = case maybeStats of Just _ -> True; Nothing -> False
    -- Pretty print individual file coverage
    ppFile (srcPath, srcLines) =
      let runtimeLines = fromMaybe mempty $ Map.lookup srcPath runtimeLinesMap
          marked = markLines fileType hasStats srcLines runtimeLines (fromMaybe Map.empty (Map.lookup srcPath covLines))
      in T.unlines (changeFileName srcPath : changeFileLines (V.toList marked))
    topHeader = case fileType of
      Lcov -> "TN:\n"
      Html -> "<style> code { white-space: pre-wrap; display: block; background-color: #eee; }" <>
              ".e { background-color: #afa; }" <> -- executed
              ".r { background-color: #ffa; }" <> -- reverted
              ".u { background-color: #faa; }" <> -- unexecuted
              ".n { background-color: #eee; }" <> -- neutral
              "</style>"
      Txt  -> ""
    -- ^ Text to add to top of the file
    changeFileName (T.pack -> fn) = case fileType of
      Lcov -> "SF:" <> fn
      Html -> "<b>" <> HTML.text fn <> "</b>"
      Txt  -> fn
    -- ^ Alter file name, in the case of html turning it into bold text
    changeFileLines ls = case fileType of
      Lcov -> ls ++ ["end_of_record"]
      Html -> legendLine : "<code>" : ls ++ ["", "</code>","<br />"]
      Txt  -> ls
    -- ^ Alter file contents, in the case of html encasing it in <code> and adding a line break
    legendLine = if hasStats
      then "<br /><b>Legend:</b> Line # | Execs # | Reverts # | Code<br />"
      else ""
  in topHeader <> T.unlines (map ppFile allFiles)

-- | Mark one particular line, from a list of lines, keeping the order of them
markLines :: CoverageFileType -> Bool -> V.Vector Text -> S.Set Int -> Map Int ([TxResult], StatsInfo) -> V.Vector Text
markLines fileType hasStats codeLines runtimeLines resultMap =
  V.map markLine . V.filter shouldUseLine $ V.indexed codeLines
  where
  shouldUseLine (i, _) = case fileType of
    Lcov -> i + 1 `elem` runtimeLines
    _ -> True
  markLine (i, codeLine) =
    let n = i + 1
        (results, (execs, reverts)) = fromMaybe ([], (0, 0)) (Map.lookup n resultMap)
        markers = sort $ nub $ getMarker <$> results
        wrapLine :: Text -> Text
        wrapLine line = case fileType of
          Html -> "<span class='" <> cssClass <> "'>" <>
                        HTML.text line <>
                     "</span>"
          _ -> line
          where
          cssClass = if n `elem` runtimeLines then getCSSClass markers else "n" -- fallback to 'neutral' class.
        result = case fileType of
          Lcov -> pack $ printf "DA:%d,%d" n execs
          Html | hasStats -> pack $ printf "%*d | %4s | %4s | %-4s| %s" lineNrSpan n (prettyCount execs) (prettyCount reverts) markers (wrapLine codeLine)
          _    -> pack $ printf " %*d | %-4s| %s" lineNrSpan n markers (wrapLine codeLine)

    in result
  lineNrSpan = length . show $ V.length codeLines + 1
  -- Pretty print large numbers with SI suffixes (k, M, G, etc.)
  prettyCount :: Word64 -> String
  prettyCount x = prettyCount' x 0
  prettyCount' x n | x >= 1000          = prettyCount' (x `div` 1000) (n + 1)
                   | x < 1000 && n == 0 = show x
                   | otherwise          = show x <> [" kMGTPEZY" !! n]

getCSSClass :: String -> Text
getCSSClass markers =
  case markers of
   []                      -> "u" -- unexecuted
   _  | '*' `elem` markers -> "e" -- executed
   _                       -> "r" -- reverted

-- | Select the proper marker, according to the result of the transaction
getMarker :: TxResult -> Char
getMarker ReturnTrue    = '*'
getMarker ReturnFalse   = '*'
getMarker Stop          = '*'
getMarker ErrorRevert   = 'r'
getMarker ErrorOutOfGas = 'o'
getMarker _             = 'e'

-- | Given a source cache, a coverage map, optionally a stats map, and contracts,
-- returns a map of covered lines with their results and stats
srcMapCov :: SourceCache -> FrozenCoverageMap -> Maybe FrozenStatsMap -> [SolcContract] -> Map FilePath (Map Int ([TxResult], StatsInfo))
srcMapCov sc covMap maybeStatsMap contracts =
  Map.unionsWith Map.union $ linesCovered <$> contracts
  where
  linesCovered :: SolcContract -> Map FilePath (Map Int ([TxResult], StatsInfo))
  linesCovered c =
    case Map.lookup c.runtimeCodehash covMap of
      Just vec -> VU.foldl' (\acc covInfo -> case covInfo of
        (-1, _, _) -> acc -- not covered
        (opIx, _stackDepths, txResults) ->
          case srcMapForOpLocation c opIx of
            Just srcMap ->
              case srcMapCodePos sc srcMap of
                Just (file, line) ->
                  Map.alter
                    (Just . innerUpdate . fromMaybe mempty)
                    file
                    acc
                  where
                  innerUpdate = Map.alter updateLine line
                  updateLine (Just (r, s)) = Just ((<> unpackTxResults txResults) r, maxStats s idxStats)
                  updateLine Nothing = Just (unpackTxResults txResults, idxStats)
                  -- Get stats for this opIx from the stats map
                  fileStats = maybeStatsMap >>= Map.lookup c.runtimeCodehash
                  idxStats = maybe (0, 0) (\sv -> if opIx >= 0 && opIx < VU.length sv then sv VU.! opIx else (0, 0)) fileStats
                  maxStats (a1, b1) (a2, b2) = (max a1 a2, max b1 b2)
                Nothing -> acc
            Nothing -> acc
        ) mempty vec
      Nothing -> mempty

-- | Given a contract, and tuple as coverage, return the corresponding mapped line (if any)
srcMapForOpLocation :: SolcContract -> OpIx -> Maybe SrcMap
srcMapForOpLocation contract opIx =
  Seq.lookup opIx (contract.runtimeSrcmap <> contract.creationSrcmap)

-- | Builds a Map from file paths to lines that can be executed, this excludes
-- for example lines with comments
buildRuntimeLinesMap :: SourceCache -> [SolcContract] -> Map FilePath (S.Set Int)
buildRuntimeLinesMap sc contracts =
  Map.fromListWith (<>)
    [(k, S.singleton v) | (k, v) <- mapMaybe (srcMapCodePos sc) srcMaps]
  where
  srcMaps = concatMap
    (\c -> toList $ c.runtimeSrcmap <> c.creationSrcmap) contracts

-- | Check that all assertions were hit, and log a warning if they weren't
checkAssertionsCoverage
  :: SourceCache
  -> Env
  -> IO ()
checkAssertionsCoverage sc env = do
  covMap <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
  let
    cs = Map.elems env.dapp.solcByName
    asserts = maybe [] (concatMap assertLocationList . Map.elems . (.asserts)) env.slitherInfo
    -- Pass Nothing for stats since we don't need them for assertion checking
    covLines = srcMapCov sc covMap Nothing cs
  mapM_ (checkAssertionReached covLines) asserts

-- | Helper function for `checkAssertionsCoverage` which checks a single assertion
-- and logs a warning if it wasn't hit
checkAssertionReached :: Map String (Map Int ([TxResult], StatsInfo)) -> AssertLocation -> IO ()
checkAssertionReached covLines assert =
  maybe
    warnAssertNotReached checkCoverage
    (Map.lookup assert.filenameAbsolute covLines)
  where
   checkCoverage coverage = let lineNumbers = Map.keys coverage in
     unless (NE.head assert.assertLines `elem` lineNumbers) warnAssertNotReached
   warnAssertNotReached =
    putStrLn $ "WARNING: assertion at file: " ++ assert.filenameRelative
       ++ " starting at line: " ++ show (NE.head assert.assertLines) ++ " was never reached"

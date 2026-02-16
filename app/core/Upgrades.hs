{-# LANGUAGE TupleSections #-}
module Core.Upgrades where

import Artifact
import Character
import Core.Utils (toArtifactInfo, aiOriginal, BuildInfo, ArtifactInfo (..))
import Core.SearchEngine (bestBuildInfo)
import Core.Progression (progression, progressionInfo)
import ImportGOOD (readGOODLevelled)
import Generator (generateArtifacts)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sortBy, sort, foldl')
import Data.Ord (comparing, Down(..))
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Core.Pareto
import Data.Array

buildSearch :: Character -> [ArtifactInfo] -> [ArtifactInfo] -> BuildInfo
buildSearch = bestBuildInfo 7

-- | Main entry point: loads artifacts, runs simulation, prints incremental results
-- Returns final statistics for recording/further processing
simulateUpgrades :: Character -> String -> String -> Int -> Int -> IO [UpgradeStats]
simulateUpgrades char goodFilePath setName genCount numRuns = do
  -- Load and filter artifacts (readGOODLevelled already filters level 20)
  allArtifacts <- readGOODLevelled goodFilePath
  let (realOnSet, realOffSet) = partitionBySet setName allArtifacts
      -- Convert to ArtifactInfo immediately
      realOnSetInfo = map (toArtifactInfo char) realOnSet
      realOffSetInfo = map (toArtifactInfo char) realOffSet

  -- Run simulations with incremental reporting
  finalStats <- runSimulationsIncremental char realOnSetInfo realOffSetInfo
                  setName genCount numRuns

  -- TODO: Convert finalStats back to Artifact for display
  return finalStats

-- | Run N simulations, printing statistics after each
-- Accumulates tracking maps and updates display incrementally
runSimulationsIncremental :: Character -> [ArtifactInfo] -> [ArtifactInfo]
                          -> String -> Int -> Int -> IO [UpgradeStats]
runSimulationsIncremental char realOnSetInfo realOffSetInfo setName genCount numRuns =
  loop 1 []
  where
    paretoRaw = paretoUnsafeAccum (scaling char) (aiStatline . snd)
    pareto flt fwd items = (fwdResult, fullResult)
      where
        fltByPiece = partitionOnPiece (aiPiece . snd) flt
        fwdByPiece = partitionOnPiece (aiPiece . snd) fwd
        itemsByPiece = partitionOnPiece (aiPiece . snd) items
        allPieces = indices itemsByPiece
        results = [paretoRaw (lookup p fltByPiece) (lookup p fwdByPiece) (itemsByPiece ! p)
                  | p <- allPieces]
        lookup p arr = if inRange (bounds arr) p then arr ! p else []
        fwdResult = concatMap fst results
        fullResult = concatMap snd results
    (_, onRealFlt) = pareto [] [] (map (0,) realOnSetInfo)
    (tmp, realMix) = pareto onRealFlt [] (map (0,) realOffSetInfo)
    (offRealFlt, _) = pareto onRealFlt [] (reverse tmp)
    allRealInfo = map snd (onRealFlt ++ offRealFlt)
    initialBuildInfo = buildSearch char (map snd onRealFlt) (map snd offRealFlt)

    -- Internal: run single progression with boundaries
    -- Returns progression with (0, initialBuild) prepended and (2*genCount, finalBuild) appended
    runSingleProgression :: [(Int, ArtifactInfo)] -> [(Int, ArtifactInfo)] -> [(Int, BuildInfo)]
    runSingleProgression genOnSetIndexed genOffSetIndexed =
      (0, initialBuildInfo) : progResults ++ [(sentinelIdx, finalBuildInfo)]
      where
        (onSetAllFwd, _) = pareto onRealFlt onRealFlt genOnSetIndexed
        (offSetAllFwd, _) = pareto realMix offRealFlt genOffSetIndexed
        processedOnSet = sortBy (comparing (Down . fst)) onSetAllFwd
        processedOffSet = sortBy (comparing (Down . fst)) offSetAllFwd

        -- Run core progression
        progResults = progressionInfo char buildSearch processedOnSet processedOffSet

        -- Sentinel index: 2x genCount (estimate for "never replaced")
        sentinelIdx = 2 * genCount

        -- Final build: last build from progression, or initial if no changes
        finalBuildInfo = if null progResults
                         then initialBuildInfo
                         else snd (last progResults)

    loop :: Int -> [Map ArtifactInfo Int] -> IO [UpgradeStats]
    loop n accMaps
      | n > numRuns = return $ aggregateStats allRealInfo accMaps
      | otherwise = do
          -- Generate indexed artifacts
          (genOnSetIndexed, genOffSetIndexed) <- generateIndexedArtifacts char setName genCount

          -- Run progression
          let progResult = runSingleProgression genOnSetIndexed genOffSetIndexed
              lastSeenMap = trackLastSeen progResult
              updatedMaps = lastSeenMap : accMaps

          -- Print progress
          printProgressUpdate n numRuns

          -- Print current statistics
          let currentStats = aggregateStats allRealInfo updatedMaps
          putStrLn $ formatStatsTable currentStats
          putStrLn ""

          -- Continue
          loop (n+1) updatedMaps

-- | Generate N artifacts for both on-set and off-set, indexed 1..N
-- Returns indexed ArtifactInfo ready for progressionInfo
generateIndexedArtifacts :: Character -> String -> Int -> IO ([(Int, ArtifactInfo)], [(Int, ArtifactInfo)])
generateIndexedArtifacts char setName count = do
  onSetArts <- generateArtifacts setName count
  offSetArts <- generateArtifacts "off set" count
  let onSetInfo = map (toArtifactInfo char) onSetArts
      offSetInfo = map (toArtifactInfo char) offSetArts
      onSetIndexed = zip [1..] onSetInfo
      offSetIndexed = zip [1..] offSetInfo
  return (onSetIndexed, offSetIndexed)

-- | Extract last-seen index for each artifact across all builds
-- Key insight: Map.insert overwrites, so later appearances update the index
trackLastSeen :: [(Int, BuildInfo)] -> Map ArtifactInfo Int
trackLastSeen snapshots = foldl' updateWithBuild Map.empty snapshots
  where
    updateWithBuild :: Map ArtifactInfo Int -> (Int, BuildInfo) -> Map ArtifactInfo Int
    updateWithBuild acc (idx, buildInfo) =
      foldl' (\m artInfo -> Map.insert artInfo idx m) acc buildInfo

-- | Statistics for a single artifact across multiple runs
data UpgradeStats = UpgradeStats
  { usArtifact :: ArtifactInfo       -- The artifact itself (as ArtifactInfo)
  , usReplacementIndices :: [Int]    -- Last-seen indices from each run
  , usMedianReplacement :: Int       -- Median of replacement indices
  , usProbability :: Double          -- Fraction of runs where artifact appeared
  , usRating :: Double               -- median * probability (sort key)
  } deriving (Show)

-- | Aggregate tracking maps into statistics for all real artifacts
-- Artifacts never appearing in any build get empty replacement indices
aggregateStats :: [ArtifactInfo] -> [Map ArtifactInfo Int] -> [UpgradeStats]
aggregateStats allRealInfo trackingMaps = map computeStats allRealInfo
  where
    numRuns = length trackingMaps

    computeStats :: ArtifactInfo -> UpgradeStats
    computeStats artInfo = UpgradeStats
      { usArtifact = artInfo
      , usReplacementIndices = appearances
      , usMedianReplacement = med
      , usProbability = prob
      , usRating = fromIntegral med * prob
      }
      where
        appearances = mapMaybe (Map.lookup artInfo) trackingMaps
        med = if null appearances then 0 else median appearances
        prob = if numRuns == 0 then 0.0 else fromIntegral (length appearances) / fromIntegral numRuns

    median :: [Int] -> Int
    median xs = sort xs !! (length xs `div` 2)

-- | Format statistics as pretty table
-- Filters out artifacts with probability = 0 (never useful)
-- Sorts by rating descending (hardest to replace first)
formatStatsTable :: [UpgradeStats] -> String
formatStatsTable stats =
  unlines $ header : separator : dataRows
  where
    -- Filter and sort
    filtered = filter ((> 0) . usProbability) stats
    sorted = sortBy (comparing (Down . usRating)) filtered

    -- Header
    header = padRight 8 "Rating" ++ " | " ++
             padRight 8 "Median" ++ " | " ++
             padRight 6 "Prob" ++ " | " ++
             "Artifact"
    separator = replicate 8 '-' ++ "-+-" ++
                replicate 8 '-' ++ "-+-" ++
                replicate 6 '-' ++ "-+-" ++
                replicate 40 '-'

    -- Data rows
    dataRows = map formatRow sorted

    formatRow :: UpgradeStats -> String
    formatRow us =
      padRight 8 (show $ round $ usRating us) ++ " | " ++
      padRight 8 (show $ usMedianReplacement us) ++ " | " ++
      padRight 6 (printf "%.2f" $ usProbability us) ++ " | " ++
      formatArtifact (usArtifact us)

    -- Format artifact: Piece MainStat CV:XX.X
    formatArtifact :: ArtifactInfo -> String
    formatArtifact artInfo =
      let art = aiOriginal artInfo
      in show (piece art) ++ " " ++
         "CV:" ++ printf "%.1f" (artCV art)

    padRight :: Int -> String -> String
    padRight n s = s ++ replicate (n - length s) ' '

-- | Print progress indicator between runs
printProgressUpdate :: Int -> Int -> IO ()
printProgressUpdate current total =
  putStrLn $ "=== Completed run " ++ show current ++ "/" ++ show total ++ " ==="

-- | Partition artifacts by set name into on-set vs off-set
partitionBySet :: String -> [Artifact] -> ([Artifact], [Artifact])
partitionBySet setName arts = (onSet, offSet)
  where
    onSet = filter (\a -> set a == setName) arts
    offSet = filter (\a -> set a /= setName) arts
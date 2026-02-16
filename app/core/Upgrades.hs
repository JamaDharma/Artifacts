{-# LANGUAGE TupleSections #-}
module Core.Upgrades where

import Artifact
import Character
import Core.Utils (toArtifactInfo, aiOriginal, BuildInfo, ArtifactInfo)
import Core.SearchEngine (bestBuildInfo)
import Core.Progression (progression)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sortBy, sort)
import Data.Ord (comparing, Down(..))
import Data.Maybe (mapMaybe)

-- | Main entry point: loads artifacts, runs simulation, prints incremental results
-- Returns final statistics for recording/further processing
simulateUpgrades :: Character -> String -> String -> Int -> Int -> IO [UpgradeStats]
simulateUpgrades char goodFilePath setName genCount numRuns = do
  -- Load and filter artifacts
  allArtifacts <- undefined -- TODO: loadGOOD goodFilePath
  let level20Arts = filter isMaxLevel allArtifacts
      (realOnSet, realOffSet) = partitionBySet setName level20Arts
  
  -- Compute initial build once (deterministic)
  let initialBuild = map aiOriginal $ bestBuildInfo char 
                       (map (toArtifactInfo char) realOnSet)
                       (map (toArtifactInfo char) realOffSet)
  
  putStrLn $ "Initial build computed. Starting " ++ show numRuns ++ " runs..."
  putStrLn ""
  
  -- Run simulations with incremental reporting
  finalStats <- runSimulationsIncremental char realOnSet realOffSet 
                  (realOnSet ++ realOffSet) setName genCount numRuns
  
  return finalStats

-- | Run N simulations, printing statistics after each
-- Accumulates tracking maps and updates display incrementally
runSimulationsIncremental :: Character -> [Artifact] -> [Artifact] -> [Artifact]
                          -> String -> Int -> Int -> IO [UpgradeStats]
runSimulationsIncremental char realOnSet realOffSet allRealArts setName genCount numRuns = 
  loop 1 []
  where
    -- Compute initial build once for probability calculation
    initialBuild = map aiOriginal $ bestBuildInfo char 
                     (map (toArtifactInfo char) realOnSet)
                     (map (toArtifactInfo char) realOffSet)
    
    loop :: Int -> [Map Artifact Int] -> IO [UpgradeStats]
    loop n accMaps
      | n > numRuns = return $ aggregateStats allRealArts accMaps
      | otherwise = do
          -- Run single progression
          let (genOnSet, genOffSet) = generateIndexedArtifacts char setName genCount
              progResult = runSingleProgression char initialBuild 
                             realOnSet realOffSet genOnSet genOffSet genCount
              lastSeenMap = trackLastSeen progResult
              updatedMaps = lastSeenMap : accMaps
          
          -- Print progress
          printProgressUpdate n numRuns
          
          -- Print current statistics
          let currentStats = aggregateStats allRealArts updatedMaps
          putStrLn $ formatStatsTable currentStats
          putStrLn ""
          
          -- Continue
          loop (n+1) updatedMaps

-- | Run single progression with boundary conditions
-- Returns progression with (0, initialBuild) prepended and (2*genCount, finalBuild) appended
runSingleProgression :: Character -> Build -> [Artifact] -> [Artifact] 
                     -> [Artifact] -> [Artifact] -> Int -> [(Int, Build)]
runSingleProgression char initialBuild realOnSet realOffSet genOnSet genOffSet genCount =
  (0, initialBuild) : progResults ++ [(sentinelIdx, finalBuild)]
  where
    -- Run core progression (skips index 0, stops at last change)
    progResults = progression char bestBuildInfo 
                    (realOnSet ++ genOnSet) (realOffSet ++ genOffSet)
    
    -- Sentinel index: 2x genCount (estimate for "never replaced")
    sentinelIdx = 2 * genCount
    
    -- Final build: last build from progression, or initial if no changes
    finalBuild = if null progResults 
                 then initialBuild
                 else snd (last progResults)

-- | Generate N artifacts for both on-set and off-set
-- Uses Generator.generateArtifactForPiece under the hood
generateIndexedArtifacts :: Character -> String -> Int -> ([Artifact], [Artifact])
generateIndexedArtifacts char setName count = 
  undefined -- TODO: call Generator.generateArtifactForPiece
  -- Should return (onSetGenerated, offSetGenerated)
  -- On-set artifacts have `asSet = setName`
  -- Off-set artifacts can be any other set (doesn't matter which)

-- | Extract last-seen index for each artifact across all builds
-- Key insight: Map.insert overwrites, so later appearances update the index
trackLastSeen :: [(Int, Build)] -> Map Artifact Int
trackLastSeen snapshots = foldl' updateWithBuild Map.empty snapshots
  where
    updateWithBuild :: Map Artifact Int -> (Int, Build) -> Map Artifact Int
    updateWithBuild acc (idx, build) = 
      foldl' (\m art -> Map.insert art idx m) acc build

-- | Statistics for a single artifact across multiple runs
data UpgradeStats = UpgradeStats
  { usArtifact :: Artifact           -- The artifact itself
  , usReplacementIndices :: [Int]    -- Last-seen indices from each run
  , usMedianReplacement :: Int       -- Median of replacement indices
  , usProbability :: Double          -- Fraction of runs where artifact appeared
  , usRating :: Double               -- median * probability (sort key)
  } deriving (Show)

-- | Aggregate tracking maps into statistics for all real artifacts
-- Artifacts never appearing in any build get empty replacement indices
aggregateStats :: [Artifact] -> [Map Artifact Int] -> [UpgradeStats]
aggregateStats allRealArts trackingMaps = map computeStats allRealArts
  where
    numRuns = length trackingMaps
    
    computeStats :: Artifact -> UpgradeStats
    computeStats art = UpgradeStats
      { usArtifact = art
      , usReplacementIndices = appearances
      , usMedianReplacement = if null appearances then 0 else median appearances
      , usProbability = fromIntegral (length appearances) / fromIntegral numRuns
      , usRating = fromIntegral med * prob
      }
      where
        appearances = mapMaybe (`Map.lookup` art) trackingMaps
        med = if null appearances then 0 else median appearances
        prob = fromIntegral (length appearances) / fromIntegral numRuns
    
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
    formatArtifact :: Artifact -> String
    formatArtifact art = 
      show (asPiece art) ++ " " ++
      show (asMainStat art) ++ " " ++
      "CV:" ++ printf "%.1f" (undefined :: Double) -- TODO: calculate CV
    
    padRight :: Int -> String -> String
    padRight n s = s ++ replicate (n - length s) ' '
    
    printf :: String -> Double -> String
    printf = undefined -- TODO: proper formatting

-- | Print progress indicator between runs
printProgressUpdate :: Int -> Int -> IO ()
printProgressUpdate current total = 
  putStrLn $ "=== Completed run " ++ show current ++ "/" ++ show total ++ " ==="

-- | Filter artifacts to level 20 only (4+ substats = all upgrades received)
isMaxLevel :: Artifact -> Bool
isMaxLevel art = length (filter (/= NoSubStat) (asSubStats art)) >= 4

-- | Partition artifacts by set name into on-set vs off-set
partitionBySet :: String -> [Artifact] -> ([Artifact], [Artifact])
partitionBySet setName arts = (onSet, offSet)
  where
    onSet = filter (\a -> asSet a == setName) arts
    offSet = filter (\a -> asSet a /= setName) arts
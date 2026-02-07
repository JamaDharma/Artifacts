module BuildSearchComparision where

import TestData
import Control.Exception (evaluate)
import ArtifactType
import Data.List (permutations,partition)
import Data.List.Extra
import Data.Array ((!), Array, listArray, array)
import Character ( Character(name, dmgClc, scaling, stDmgClc), furina, nefer, statAccessor )
import CharacterBuild
    ( BuildStrategy(BuildStrategy, weightCalculator, character,
                    buildMaker),
      bestBuild,
      bestBuildFolding,
      partitionOnPiece,
      paretoFilter,
      paretoFilterReal,
      best4pcBuilds,
      fold4pcBuilds,
      extendWeights,
      calcStatWeightsC,
      bestBuildStrategic,
      updateWeights )
import Progression
import Generator
import ImportGOOD
import GeneratorUtils
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import UpgradeSimulator

regTestChr :: Character
regTestChr = furina

getMeasure :: Int -> Int -> IO (Double, [(Int, Double)])
getMeasure depth cases = do
    let chr = furina
    let damage = dmgClc chr []
    let bm s o = [bestBuild depth chr s o]
    let dfs = damageFromSeed damage 10000 bm
    let getResults = go where
        go [] = return []
        go (seed:t) = do
              putStr ("\r"++show seed++"      ")
              hFlush stdout
              dmg <- dfs seed
              _ <- evaluate dmg  -- Explicitly force evaluation in IO
              rest <- go t
              if dmg /= 1 then return$ (seed,dmg):rest else return$ (seed,dmg):rest
    whileMeasuringTime (getResults [0..cases])

getReport :: [Char] -> Int -> Int -> IO [Char]
getReport prefix depth cases = do
  res <- getMeasure depth cases
  let report = prefix ++"_"++show depth++"P_10K_"++show cases++"S = "++show res
  return report

measureAndRecordX :: IO Bool
measureAndRecordX = do
    let prefix = "bestBuild"
    let reporter = uncurry (getReport prefix)
    reports <- mapM reporter [(5,150),(10,50),(15,10)]
    writeFile ("data/"++prefix++"Report.hs") (unlines reports)
    return True

-- | Find minimum depth where build maker achieves target damage for a seed
-- Returns Nothing if maxDepth exceeded
findMinDepth :: (Int->BuildMaker) -> Int -> Double -> Int -> IO (Int,[Char])
findMinDepth bmm seed targetDamage maxDepth = tryDepth 1
  where
    damage = dmgClc regTestChr []
    
    tryDepth depth
        | depth > maxDepth = return (-1,"")
        | otherwise = do
            let dfs = damageFromSeed damage 10000 (bmm depth)
            dmg <- dfs seed
            if dmg > targetDamage+0.1
                then return (depth,"+")
                else if dmg < targetDamage-0.1
                    then tryDepth (depth + 1)
                    else return (depth,"")


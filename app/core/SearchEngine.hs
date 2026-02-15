module Core.SearchEngine where

import Artifact
import Character
import Statline
import Core.Utils
import Core.Pareto
import Core.Traversal

import Data.Ord (comparing, Down(..))
import Data.Array
import Data.List (maximumBy, minimumBy, foldl', sortOn, intercalate)
import Data.List.Extra (groupSortOn)
import Debug.Trace
import Text.Printf (printf)

-- CORE TYPES
type ArtifactStorage = ([Artifact], [Artifact])

-- STATLINE HELPERS
-- Convert BuildInfo to complete Statline (char stats + artifact stats)
buildInfoToStatline :: Character -> BuildInfo -> Statline
buildInfoToStatline c buildInfo = addStatlines charStats artifactStats where
  charStats = collectStatsNormalized c (displS c ++ bonusS c)
  artifactStats = foldl' addStatlines zeroStatline (map aiStatline buildInfo)

-- Build complete statlines from BuildInfo list
buildStatlinesInfo :: Character -> [BuildInfo] -> [Statline]
buildStatlinesInfo c = map (buildInfoToStatline c)

-- Apply single-stat buff to statline (buff in rolls, converted to value)
buffStatline :: Statline -> Stat -> Double -> Statline
buffStatline sl s rolls = appendStats sl [statRollToValue (s, rolls)]

-- WEIGHT CALCULATION
-- Calculate sensitivity: damage change per roll of stat
-- Uses ±17 rolls (≈2 good rolls) to measure slope
calcSensitivity :: (Statline -> Double) -> [Statline] -> Stat -> Double
calcSensitivity dmgCalc statlines s = (plusDmg - minusDmg) / baseDmg * 100 / 4 where
  baseDmg = maximum (map dmgCalc statlines)
  buffed rolls = map (\sl -> dmgCalc (buffStatline sl s rolls)) statlines
  plusDmg = maximum (buffed 17)
  minusDmg = maximum (buffed (-17))

-- Balanced weights updater - simple sensitivity analysis
calcStatWeightsBInfo :: Character -> [BuildInfo] -> [(Stat, Double)] -> [(Stat, Double)]
calcStatWeightsBInfo c builds = map updateW where
  dmgCalc = statlineDamageCalculator c
  statlines = buildStatlinesInfo c builds
  updateW (s, _) = (s, calcSensitivity dmgCalc statlines s)

calcStatWeightsStatlines :: Character -> [Statline] -> [(Stat, Double)] -> [(Stat, Double)]
calcStatWeightsStatlines c statlines = map updateW where
  dmgCalc = statlineDamageCalculator c
  updateW (s, _) = (s, calcSensitivity dmgCalc statlines s)

-- Constraint-aware weights calculation
constraintRange :: (Ord a, Fractional a) => a -> a -> a -> (a, a)
constraintRange minS maxS cv
  | cv-range/2 <= minS = (minS,minS + range)
  | cv+range/2 >= maxS = (maxS - range, maxS)
  | otherwise = (cv - range/2, cv + range/2)
  where range = (maxS - minS)/2

constraintSlope :: Character -> [Statline] -> (Stat,Double) -> (Stat,Double)
constraintSlope c statlines (cs,cv) = (cs, if dmg minRD == dmg maxRD then cv else dDmg/dAvgRolls) where
  -- Extract min/max value of stat cs across all builds
  getStat sl = statAccessor sl cs
  minS = minimumBy (comparing getStat) statlines
  maxS = maximumBy (comparing getStat) statlines
  (minR,maxR) = constraintRange (getStat minS) (getStat maxS) cv
  -- Find best build at each range boundary
  dmg = stDmgClcUnc c
  maxDamageR r = maximumBy (comparing dmg) . filter ((>=r).getStat) $ statlines
  minRD = maxDamageR minR
  maxRD = maxDamageR maxR
  -- Calculate sensitivity
  (_, dRollValue) = statValueToRoll (cs, getStat maxRD - getStat minRD)
  dAvgRolls = dRollValue/8.5
  dDmg = (dmg minRD - dmg maxRD)/(dmg minRD + dmg maxRD)*2*100

-- Constraints weights updater - experimental
-- Uses constraint-aware analysis when character has stat constraints (e.g., Furina HP threshold)
-- Falls back to all builds if no valid builds exist, allowing weight estimation even with poor gear
calcStatWeightsCInfo :: Character -> [BuildInfo] -> [(Stat,Double)] -> [(Stat,Double)]
calcStatWeightsCInfo c builds = map updateW where
  allStatlines = buildStatlinesInfo c builds
  validStatlines = filter (conditionChecker c) allStatlines
  -- Use valid builds if any, else all (enables weight calc even when no builds meet constraints)
  statlines = if null validStatlines then allStatlines else validStatlines
  dmgCalc = stDmgClcUnc c
  updateW (s, oldW)
    | null cnd = (s, calcSensitivity dmgCalc statlines s)
    | otherwise = constraintSlope c allStatlines (head cnd)
    where cnd = filter ((==s).fst) (condition c)

-- UTILITY
maxDamageInfo :: Character -> [BuildInfo] -> Double
maxDamageInfo c = maximum . map (dmgClc c [] . map aiOriginal)

-- MAIN OPTIMIZATION ENTRY POINTS
-- bestBuildInfo: iterative weight refinement
bestBuildInfo :: Int -> Character -> [ArtifactInfo] -> [ArtifactInfo] -> BuildInfo
bestBuildInfo n c setA offA = bb where
  bmkr w = best4pcStatlines c w n setA offA
  rollW = zip (scaling c) [1,1..]
  wl = defaultWeightline c
  firsts = bmkr wl
  (bb,_) = go firsts rollW
  -- Helper
  showRW ws = intercalate ", " [show s ++ ":" ++ printf "%.2f" w | (s,w) <- ws]
  showAll cm (d, w) = printf "%s Damage: %.0f, Weights: %s" cm d (showRW w)
  go ((oldBest, oldMax), oldBuilds) oldRollW
    | newMax > oldMax = --trace (showAll "Improved -" (oldMax, oldRollW))$
      go news newRollW
    | oldMax < 1 = ([], oldRollW)
    | otherwise = --trace (showAll "Improved -" (oldMax, oldRollW))$trace (showAll "Failed to -" (newMax, newRollW))$
      (oldBest, oldRollW)
    where
      newRollW = calcStatWeightsStatlines c oldBuilds oldRollW
      newWL = rollsToWeightline newRollW
      news@((_, newMax), _) = bmkr newWL

-- Precompute buff storage statline
makeBuffStatline :: [Stat] -> Double -> Statline
makeBuffStatline stats rolls = 
  appendStats zeroStatline converted
  where
    converted = map (\s -> statRollToValue (s, rolls)) stats
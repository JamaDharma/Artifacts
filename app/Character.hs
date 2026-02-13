module Character(
  Character(..),
  getShownStat,
  getFinalStatline,
  getScalingStatline,
  conditionChecker,
  statlineDamageCalculator,
) where

import Artifact
import Statline

import Data.Array

data Character = Character
  { name     :: String
  , scaling  :: [Stat]
  , baseS    :: Array Stat Double--base values flat HP ATK DEF
  , displS   :: [(Stat, Double)]--values
  , bonusS   :: [(Stat, Double)]--values
  , condition:: [(Stat, Double)]--minimum values
  , dmgClc   :: [(Stat,Double)]->Build->Double
  , stDmgClcUnc :: Statline->Double
  }

-- Combine stats into an Array Stat Double, summing values and using 0.0 as default
combineStats :: [(Stat, Double)] -> Array Stat Double
combineStats = accumArray (+) 0.0 (HPf, DMGb)
addStats :: Array Stat Double -> [(Stat, Double)] -> Array Stat Double
addStats = accum (+)

getShowStatline :: Character->Build->Array Stat Double
getShowStatline c = combineStats.concat.(displS c:).map stats

getFinalStatline :: Character->Build->Array Stat Double
getFinalStatline c = combineStats.concat.(charStats:).map stats
  where charStats = displS c ++ bonusS c

getShownStat::Character->Build->Stat->Double
getShownStat c arts s
  | inRange (HP,DEF) s = statline!(flatMap!s) + (baseS c)!s * (1+statline!s/100)
  | inRange (ER,DMG) s = statline!s
  | otherwise = error ("Invalid stat" ++ show s)
  where statline = getShowStatline c arts

getScalingStatline :: Character->Artifact->Array Stat Double
getScalingStatline c a = combineStats (map fIncS.scaling $ c) where
  fIncS s
    | inRange (HP,DEF) s = (s, sl!s + sl!(flatMap!s)/(baseS c)!s*100)
    | otherwise = (s,sl!s)
  sl = combineStats (stats a)

conditionChecker :: Character -> Statline -> Bool
conditionChecker c sl = all (\(cs,cv)->statAccessor sl cs >= cv).condition$c
statlineDamageCalculator :: Character->Statline->Double
statlineDamageCalculator c sl = if conditionChecker c sl then stDmgClcUnc c sl else 0
module Artifact (
    Build, Piece(..), Stat(..), Artifact(..),
    flatMap,pcntToFlatW,baseStats,artCV,artRV,buildCV,buildV,
    mainStatValue, statRollToValue, statValueToRoll
    ) where

import Data.Array

data Piece = Flower|Plume|Goblet|Sands|Circlet deriving (Show,Eq,Ord,Enum,Bounded,Ix)
data Stat = HPf|ATKf|DEFf|HP|ATK|DEF|ER|EM|CR|CD|HB|DMG|DMGb deriving (Show,Eq,Ord,Enum,Bounded,Ix)

type Build = [Artifact]

baseStats :: [(Stat, e)] -> Array Stat e
baseStats = array (HP,DEF)

statValue :: Array Stat Double
statValue = array (HPf, DMGb) [
    (HPf,298.75),(ATKf,19.45),(DEFf,23.15),
    (HP,5.83),(ATK,5.83),(DEF,7.29),
    (ER,6.48),(EM,23.31),(CR,3.89),(CD,7.77),
    (HB,4.49),(DMG,5.83),(DMGb,5.83)
    ]

flatMap :: Array Stat Stat
flatMap = baseStats [(HP, HPf), (ATK, ATKf), (DEF, DEFf)]

pcntToFlatW :: Array Stat Double -> (Stat, Double) -> (Stat, Double)
pcntToFlatW bv (s,w) = (fs, w*statValue!fs/(bv!s*statValue!s/100)) where fs = flatMap!s

--1 substat rolls from 7 to 10 
statRollToValue :: (Stat,Double)->(Stat,Double)
statRollToValue (s,r) = (s,r/10*statValue!s)
statValueToRoll :: (Stat,Double)->(Stat,Double)
statValueToRoll (s,v) = (s,v*10/statValue!s)

mainStatValue :: Stat->(Stat,Double)
mainStatValue = statRollToValue.msToRoll where 
    msToRoll ms
        | ms == DEFf = error "Invalid main stat"
        | ms `elem` [HPf,ATKf] = (ms,160)
        | otherwise = (ms,80)

data Artifact = Artifact
  { piece    :: Piece
  , set    :: String
  , upNumber :: Int
  , stats    :: [(Stat, Double)]
  } deriving (Show, Eq)

sValue:: Stat->[(Stat,Double)]->Double
sValue s = sum.map snd.filter ((s==).fst)

cv::[(Stat, Double)] -> Double
cv sl = 2*sValue CR sl + sValue CD sl

artCV:: Artifact->Double
artCV = cv.tail.stats

artRV:: Artifact->Int
artRV = sum.map (round.snd.statValueToRoll).tail.stats

buildCV:: [Artifact]->Double
buildCV = cv.concatMap stats

buildV:: Stat->[Artifact]->Double
buildV s = sValue s.concatMap stats

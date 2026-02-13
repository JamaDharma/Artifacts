module CharacterBuildLegacy (bestBuildLegacy) where

import Artifact
import Character(Character(..))
import Data.Ord (comparing, Down(..))
import Data.Array
import Data.List (maximumBy)
import Data.List.Extra (sortOn) -- Ensure you have "extra" package or import sortOn from Data.List if using GHC >= 7.10
import Data.Ix (inRange)
--Auxiliary functions from other nmodules
--Statline for efficient access to agregate character stats
type Statline = Array Stat Double
statAccessor :: Statline -> Stat -> Double
statAccessor = (!)
statDecorator :: Statline -> (Stat, Double) -> Stat -> Double
statDecorator sl (bs,bv) s --accessor decorated with single buff
  | s == bs = sl ! s + bv
  | otherwise = sl ! s
collectStats :: [(Stat, Double)] -> Statline
collectStats = accumArray (+) 0.0 (HPf, DMGb)
appendStats :: Statline -> [(Stat, Double)] -> Statline
appendStats = accum (+)

-- Combine stats into an Array Stat Double, summing values and using 0.0 as default
combineStats :: [(Stat, Double)] -> Array Stat Double
combineStats = accumArray (+) 0.0 (HPf, DMGb)
addStats :: Array Stat Double -> [(Stat, Double)] -> Array Stat Double
addStats = accum (+)

conditionChecker :: Character -> (Stat -> Double) -> Bool
conditionChecker c sla = all (\(cs,cv)->sla cs >= cv).condition$c
stDmgClc2 :: Character -> (Stat -> Double) -> Double
stDmgClc2 c sla = furinaStatlineDmgClc c sla
furinaStatlineDmgClc :: Character->((Stat->Double)->Double)
furinaStatlineDmgClc c = innerDmg where
  baseHP = baseS c!HP
  defMlt = 190/390
  baseMV = 14.92/100*1.4*0.9*defMlt
  innerDmg sl = dmgOutput  where
    effHp = sl HPf+baseHP*(1+sl HP/100)
    dmgOutput = effHp*baseMV*simpleMult sl
simpleMult :: (Stat->Double) -> Double
simpleMult sl = critMlt*dmgMlt where
  eCR = max 0 (min 100 (sl CR))/100
  critMlt = 1 + eCR*sl CD/100
  dmgMlt = 1 + sl DMG/100
characterDamageCalculator :: Character->[(Stat,Double)]->(Build->Double)
characterDamageCalculator c buff = buildDmg where
  sdc = stDmgClc2 c
  buffStatline = combineStats (displS c ++ bonusS c ++ buff)
  buildDmg build = if conditionChecker c sla then sdc sla else 0
    where sla = ((addStats buffStatline.concatMap stats$build)!)

-- Main Entry Point
bestBuildLegacy :: Int -> Character -> [Artifact] -> [Artifact] -> Build
bestBuildLegacy n c setA offA = bb where
  dmg = characterDamageCalculator c []
  -- Helper to make builds based on weights
  bmkr w = best4pcBuilds (extendWeights c w) n setA offA
  maxBy = maximumBy . comparing

  -- Initial weights based on character scaling
  rollW = zip (scaling c) [1,1..]
  firstBuilds = bmkr rollW
  
  -- Optimization loop
  (bb,_,_) = go (maxBy dmg firstBuilds, rollW, firstBuilds)

  go (oldBest, oldSW, oldBuilds)
    | dmg newMax > dmg oldBest = go (newMax, newSW, newBuilds)
    -- Safety check for extremely low damage (prevent infinite loops on empty/invalid builds)
    | dmg oldBest < 1 = ([], oldSW, oldBuilds) 
    | otherwise = (oldBest, oldSW, oldBuilds)
    where
      newSW = calcStatWeightsB c oldBuilds oldSW
      newBuilds = bmkr newSW
      newMax = maxBy dmg newBuilds

-- Balanced weights updater (Recursive step logic)
calcStatWeightsB :: Character -> [Build] -> [(Stat,Double)] -> [(Stat,Double)]
calcStatWeightsB c builds = map updateW where
  -- Use stDmgClc for internal weight calculation
  calc sla = if conditionChecker c sla then stDmgClc2 c sla else 0
  
  charStats = collectStats (displS c ++ bonusS c)
  buildsStats = map (statAccessor . appendStats charStats . concatMap stats) builds
  
  maxDmg buff = maximum . map bdc $ buildsStats where
    nbs = map statRollToValue buff
    bdc bs = calc accessor where accessor = buffDecorator bs nbs
    buffDecorator sl [] s = sl s
    buffDecorator sl [(bs,bv)] s
      | s == bs = sl s + bv
      | otherwise = sl s
      
  bd = maxDmg []
  -- Calculate gradient: (Dmg(+roll) - Dmg(-roll)) / BaseDmg
  -- 17*2 is 34 which is ~4 avg rolls, so we scale to 100/4 to get % per roll
  newWeight s = (maxDmg [(s,17)] - maxDmg [(s,-17)]) / bd * 100 / 4
  updateW (s,_) = (s, newWeight s)

-- Expands percentage weights to include flat stats automatically
extendWeights :: Character -> [(Stat, Double)] -> [(Stat, Double)]
extendWeights c = concatMap extend where
  extend (s,w)
    | inRange (HPf, DEFf) s = error "Flat stats in weights are added automatically"
    | inRange (HP, DEF) s = [(s,w), pcntToFlatW (baseS c) (s,w)]
    | otherwise = [(s,w)]

-- Core Build Generation Logic
best4pcBuilds :: [(Stat,Double)] -> Int -> [Artifact] -> [Artifact] -> [Build]
best4pcBuilds rollW n = offpieceBuilds (bestPieces rollW n)

offpieceBuilds :: ([Artifact] -> [[Artifact]]) -> [Artifact] -> [Artifact] -> [Build]
offpieceBuilds extractor setA offA = concatMap sequence variants where
  pieceT = piece . head
  setP = extractor setA
  offP = extractor offA
  
  -- Logic to swap one piece for an off-piece
  off p = filter ((/=p) . pieceT) setP ++ filter ((==p) . pieceT) offP
  variants = setP : map (sortOn pieceT . off . pieceT) offP

bestPieces :: [(Stat, Double)] -> Int -> [Artifact] -> [[Artifact]]
bestPieces rollW n = map takeBestN . partition where
  partition = filter (/=[]) . elems . partitionOnPiece piece
  -- statValueToRoll to convert from per roll to per value weights for sorting
  sw = statAccessor . collectStats . map statValueToRoll $ rollW
  takeBestN = take n . sortOn (Down . artValue sw)

artValue :: (Stat -> Double) -> Artifact -> Double
artValue weights a = sum [value * weights stat | (stat, value) <- stats a]

-- Partitioning Helpers
partitionOnPiece :: (a -> Piece) -> [a] -> Array Piece [a]
partitionOnPiece f = partitionOnPieceR f . reverse

partitionOnPieceR :: (a -> Piece) -> [a] -> Array Piece [a]
partitionOnPieceR f = accumArray (flip (:)) [] (Flower,Circlet) . map (\x -> (f x, x))
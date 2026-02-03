module CharacterBuild where

import ArtifactType
import Character
import Data.Ord (comparing, Down(..))
import Data.Array
import Data.List (maximumBy, minimumBy)
import Data.List.Extra (sortOn, groupSortOn, maximumOn)

type ArtifactStorage = ([Artifact], [Artifact])

data BuildStrategy = BuildStrategy {
    character :: Character,
    buildMaker :: ArtifactStorage -> [(Stat,Double)] -> [Build],
    weightCalculator :: [Build]->[(Stat,Double)]->[(Stat,Double)]
  }

defaultStrategy :: Character -> Int -> BuildStrategy
defaultStrategy c n = BuildStrategy {
    character = c,
    buildMaker = \(s,o) w -> best4pcBuilds (extendWeights c w) n s o,
    weightCalculator = calcStatWeightsB c
  }

--reverses order of elements while partitioning
partitionOnPieceR :: (a -> Piece) -> [a] -> Array Piece [a]
partitionOnPieceR f = accumArray (flip (:)) [] (Flower,Circlet).map (\x -> (f x, x))
--preserves order of elements
partitionOnPiece :: (a -> Piece) -> [a] -> Array Piece [a]
partitionOnPiece f = partitionOnPieceR f.reverse

paretoCandidatesOn::Character->(a->Artifact)->[a]->[a]
paretoCandidatesOn c f = go where
  scl = scaling c
  gss = getScalingStatline c.f
  go [] = []
  go (h:t) = h:go (filter notWorse t) where
    stlH = gss h
    notWorse a = any cmpArt scl where
      stlA = gss a
      cmpArt s = stlH!s < stlA!s

paretoFront::Character->[Artifact]->[Artifact]
paretoFront c = go where
  scl = scaling c
  gss = getScalingStatline c
  go [] = []
  go (h:t) = h:go (filter notWorse t) where
    stlH = gss h
    notWorse a = any cmpArt scl where
      stlA = gss a
      cmpArt s = stlH!s < stlA!s

allBuilds :: [Artifact] -> [Build]
allBuilds = sequence.groupSortOn piece

extendWeights :: Character -> [(Stat, Double)] -> [(Stat, Double)]
extendWeights c = concatMap extend where
  extend (s,w)
    | inRange (HPf,DEFf) s = error "Flat stats in weights are added automatically"
    | inRange (HP,DEF) s = [(s,w), pcntToFlatW (baseS c) (s,w)]
    | otherwise = [(s,w)]

artValue :: (Stat->Double) -> Artifact -> Double
artValue weights a = sum [value * weights stat | (stat, value) <- stats a]

bestPieces :: [(Stat, Double)] -> Int -> [Artifact] -> [[Artifact]]
bestPieces rollW n = map takeBestN.partition where
  partition = filter (/=[]).elems.partitionOnPiece piece
  --statValueToRoll to convert from per roll to per value weights
  sw = statAccessor.collectStats.map statValueToRoll $ rollW
  takeBestN = take n.sortOn (Data.Ord.Down. artValue sw)

paretoFilter :: Character -> [Artifact] -> [Artifact]
paretoFilter c = concatMap (paretoFront c).groupSortOn piece

paretoFilterReal :: Character -> [Artifact] -> [Artifact]
paretoFilterReal c = concatMap (pf.reverse.pf).elems.partitionOnPieceR piece
  where pf = paretoCandidatesOn c id

best4pcBuilds :: [(Stat,Double)]->Int->[Artifact]->[Artifact]->[Build]
best4pcBuilds rollW n = offpieceBuilds (bestPieces rollW n)

bestBuilds :: [(Stat,Double)]->Int->[Artifact] -> [[Artifact]]
bestBuilds rollW n = sequence.bestPieces rollW n

pieceNumMlt :: Array Piece Double
pieceNumMlt = array (Flower,Circlet)
  [(Flower,1.4),(Plume,1.4),(Sands,1),(Goblet,0.5),(Circlet,1)]
pieceAwareExtractor :: [(Stat, Double)] -> Int -> [Artifact] -> [[Artifact]]
pieceAwareExtractor rollW depth = map (takeBest.sortByVal).groupSortOn piece where
  sw = statAccessor.collectStats.map statValueToRoll $ rollW --statValueToRoll to reverse
  sortByVal = sortOn (Data.Ord.Down. artValue sw)
  takeBest l = take n l where
    p = piece (head l)
    n = round (fromIntegral depth*pieceNumMlt!p)

offpieceBuilds :: ([Artifact] -> [[Artifact]])->[Artifact]->[Artifact]->[Build]
offpieceBuilds extractor setA offA = concatMap sequence variants where
  pieceT = piece.head
  setP = extractor setA
  offP = extractor offA
  off p = filter ((/=p).pieceT) setP ++ filter ((==p).pieceT) offP
  variants = setP:map (sortOn pieceT.off.pieceT) offP

damage :: [Artifact]->Double
damage = damageWithBuff []

damageWithBuff :: [(Stat,Double)]->Build->Double
damageWithBuff = dmgClc furina

maxDamage :: Character->[Build]->Double
maxDamage c = maximum.map (dmgClc c [])

calcStatWeights :: [Build] -> [(Stat, Double)] -> [(Stat, Double)]
calcStatWeights builds = map updateW where
    maxDmg buff = maximum.map bdc $ builds where bdc = damageWithBuff buff
    bd = maxDmg []
    --34 is 4 avg rolls so 25 as 100/4 to get % per roll
    newWeight s =  maxDmg [statRollToValue (s,34)]/bd*25 - 25
    updateW (s,_) =  (s,newWeight s)

constraintRange :: (Ord a, Fractional a) => a -> a -> a -> (a, a)
constraintRange minS maxS cv
  | cv-range/2 <= minS = (minS,minS + range)
  | cv+range/2 >= maxS = (maxS - range, maxS)
  | otherwise = (cv - range/2, cv + range/2)
  where range = (maxS - minS)/2

constraintSlope :: Character->[Stat->Double]->(Stat,Double)->(Stat,Double)
constraintSlope c statlines (cs,cv) = (cs, if dmg minRD == dmg maxRD then cv else  dDmg/dAvgRolls) where--error (show [(minR,maxR),(dmg minRD,dmg maxRD),(minRD cs,maxRD cs)]) where 
  ext f = f (comparing ($ cs)) statlines
  minS = ext minimumBy
  maxS = ext maximumBy
  (minR,maxR) = constraintRange (minS cs) (maxS cs) cv

  --maximum on or by?
  dmg = stDmgClc c
  maxDamageR r = maximumBy (comparing dmg).filter ((>=r).($ cs)) $ statlines
  minRD = maxDamageR minR
  maxRD = maxDamageR maxR

  (_, dRollValue) = statValueToRoll (cs, maxRD cs - minRD cs)
  dAvgRolls = dRollValue/8.5 --8.5 is average roll value
  dDmg = (dmg minRD - dmg maxRD)/(dmg minRD + dmg maxRD)*2*100

--constraints weights updater
calcStatWeightsC :: Character->[Build]->[(Stat,Double)]->[(Stat,Double)]
calcStatWeightsC c builds = map updateW where
  calc = stDmgClc c
  charStats = collectStats (displS c ++ bonusS c)
  rawBuildsStats = map (statAccessor.appendStats charStats.concatMap stats) builds
  buildsStats = if null fb then rawBuildsStats else fb where
    fb = filter (conditionChecker c) rawBuildsStats
  maxDmg buff = maximum.map bdc $ buildsStats where
    nbs = map statRollToValue buff
    bdc bs = calc accessor where accessor = buffDecorator bs nbs
    buffDecorator sl [] s = sl s
    buffDecorator sl [(bs,bv)] s
      | s == bs = sl s + bv
      | otherwise = sl s
  bd = maxDmg []
  --17*2 is 34 which is 4 avg rolls so 100/4 to get % per roll
  --weight are relative, but useful to see when printed
  newWeight s =  (maxDmg [(s,17)]-maxDmg [(s,-17)])/bd*100/4
  updateW (s,_)
    | null cnd = (s,newWeight s)
    | otherwise = constraintSlope c rawBuildsStats (head cnd)
    where
      cnd = filter ((==s).fst) (condition c)

--balansed weights updater
calcStatWeightsB :: Character->[Build]->[(Stat,Double)]->[(Stat,Double)]
calcStatWeightsB c builds = map updateW where
  calc sla = if conditionChecker c sla then stDmgClc c sla else 0
  charStats = collectStats (displS c ++ bonusS c)
  buildsStats = map (statAccessor.appendStats charStats.concatMap stats) builds
  maxDmg buff = maximum.map bdc $ buildsStats where
    nbs = map statRollToValue buff
    bdc bs = calc accessor where accessor = buffDecorator bs nbs
    buffDecorator sl [] s = sl s
    buffDecorator sl [(bs,bv)] s
      | s == bs = sl s + bv
      | otherwise = sl s
  bd = maxDmg []
  --17*2 is 34 which is 4 avg rolls so 100/4 to get % per roll
  --weight are relative, but useful to see when printed
  newWeight s =  (maxDmg [(s,17)]-maxDmg [(s,-17)])/bd*100/4
  updateW (s,_) =  (s,newWeight s)

updateWeights2 :: Character -> Int -> [(Stat, Double)] -> [Artifact] -> [Artifact] -> (Double, [(Stat, Double)], [Build])
updateWeights2 c n rollW setA offA  = go (maxDmg firstBuilds,rollW,firstBuilds) where
  maxDmg = maxDamage c
  bmkr w = offpieceBuilds (pieceAwareExtractor (extendWeights c w) n) setA offA
  firstBuilds = bmkr rollW
  go (oldMax,oldSW,oldBuilds)
    | newMax <= oldMax = (oldMax,oldSW,oldBuilds)
    | otherwise = go (newMax,newSW,newBuilds)
    where
      newSW = calcStatWeightsB c oldBuilds oldSW
      newBuilds = bmkr newSW
      newMax = maxDmg newBuilds

updateWeights :: Character -> Int -> [(Stat, Double)] -> [Artifact] -> [Artifact] -> (Double, [(Stat, Double)], [Build])
updateWeights c n rollW setA offA  = go (maxDmg firstBuilds,rollW,firstBuilds) where
  maxDmg = maxDamage c
  bmkr w = best4pcBuilds (extendWeights c w) n setA offA
  firstBuilds = bmkr rollW
  go (oldMax,oldSW,oldBuilds)
    | newMax <= oldMax = (oldMax,oldSW,oldBuilds)
    | otherwise = go (newMax,newSW,newBuilds)
    where
      newSW = calcStatWeightsB c oldBuilds oldSW
      newBuilds = bmkr newSW
      newMax = maxDmg newBuilds

updateWeightsStrategic :: BuildStrategy -> ArtifactStorage -> [(Stat, Double)] -> (Double, [(Stat, Double)], [Build])
updateWeightsStrategic strategy storage rollW = go (maxDmg firstBuilds,rollW,firstBuilds) where
  maxDmg = maximum.map (dmgClc (character strategy) [])
  bmkr = buildMaker strategy storage
  firstBuilds = bmkr rollW
  go (oldMax,oldSW,oldBuilds)
    | newMax <= oldMax = (oldMax,oldSW,oldBuilds)
    | otherwise = go (newMax,newSW,newBuilds)
    where
      newSW = weightCalculator strategy oldBuilds oldSW
      newBuilds = bmkr newSW
      newMax = maxDmg newBuilds


bestBuildStrategic :: BuildStrategy -> ([Artifact], [Artifact]) -> Build
bestBuildStrategic strategy storage  =  bb where
  char = character strategy
  dmg = dmgClc char []
  bmkr = buildMaker strategy storage
  maxBy = maximumBy.comparing

  bb = go (-(1/0),zip (scaling char) [1,1..],[])

  go (oldMax,oldSW,oldBest)
    | newMax > oldMax = go (newMax,newSW,newBest)
    | otherwise = oldBest
    where
      newBuilds = bmkr oldSW
      newSW = weightCalculator strategy newBuilds oldSW
      newBest = maxBy dmg newBuilds
      newMax = dmg newBest

bestBuild :: Int -> Character -> [Artifact] -> [Artifact] -> Build
bestBuild n c setA offA  =  bb where
  dmg = dmgClc c []
  bmkr w = best4pcBuilds (extendWeights c w) n setA offA
  maxBy = maximumBy.comparing

  rollW = zip (scaling c) [1,1..]
  firstBuilds = bmkr rollW
  (bb,_,_) = go (maxBy dmg firstBuilds,rollW,firstBuilds)

  go (oldBest,oldSW,oldBuilds)
    | dmg newMax > dmg oldBest = go (newMax,newSW,newBuilds)
    | dmg oldBest < 1 = ([],oldSW,oldBuilds)
    | otherwise = (oldBest,oldSW,oldBuilds)
    where
      newSW = calcStatWeightsB c oldBuilds oldSW
      newBuilds = bmkr newSW
      newMax = maxBy dmg newBuilds
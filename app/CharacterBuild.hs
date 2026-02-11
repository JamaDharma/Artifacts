module CharacterBuild where

import ArtifactType
import Character
import CharacterBuildInfo (paretoFilterInfo, paretoFilterRealInfo, bestBuildInfo, bestBuildFoldingInfo)
import Data.Ord (comparing, Down(..))
import Data.Array
import Data.List (maximumBy, minimumBy, foldl')
import Data.List.Extra (sortOn, groupSortOn, maximumOn)
import StatlineType

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

-- Deprecated: O(n²) scoring for backward compatibility
artValue :: [(Stat, Double)] -> Artifact -> Double
artValue weights a = sum [value * w | (stat, value) <- stats a, (s, w) <- weights, s == stat]

bestPieces :: [(Stat, Double)] -> Int -> [Artifact] -> [[Artifact]]
bestPieces rollW n = map takeBestN.partition where
  partition = filter (/=[]).elems.partitionOnPiece piece
  --statValueToRoll to convert from per roll to per value weights. We changing 1/roll to 1/value so usual conversion meaning inverted 
  sw = map statValueToRoll rollW
  takeBestN = take n.sortOn (Data.Ord.Down. artValue sw)

-- PARETO FILTERING WRAPPERS (delegate to CharacterBuildInfo)
paretoFilter :: Character -> [Artifact] -> [Artifact]
paretoFilter c arts = map aiOriginal (paretoFilterInfo c infos)
  where infos = map (toArtifactInfo c) arts

paretoFilterReal :: Character -> [Artifact] -> [Artifact]
paretoFilterReal c arts = map aiOriginal (paretoFilterRealInfo c infos)
  where infos = map (toArtifactInfo c) arts

-- OLD FOLD INFRASTRUCTURE (has bugs - use CharacterBuildInfo instead)
-- 1. Recursive helper that accumulates Statline
foldRecursivelyS :: ((Build, Statline) -> a -> a) -> Statline -> a -> [[Artifact]] -> Build -> a
foldRecursivelyS f sl acc [] currentStack = f (currentStack, sl) acc
foldRecursivelyS f sl acc (candidates:rest) currentStack =
  foldl' processCandidate acc candidates
  where
    processCandidate acc' candidate =
      -- Optimization: Accumulate stats incrementally
      let newSL = appendStats sl (stats candidate) --bug: have to use collectStatsNormalized
      in foldRecursivelyS f newSL acc' rest (candidate:currentStack)

-- 2. Generic traversal (Fixes the type error by supplying [])
foldOffpieceBuildsS :: ([Artifact] -> [[Artifact]]) -> Character -> ((Build, Statline) -> a -> a) -> a -> [Artifact] -> [Artifact] -> a
foldOffpieceBuildsS extractor c callback initialAcc setA offA =
    foldl' processVariant initialAcc variants
  where
    -- Pre-calculate base stats once
    baseSL = collectStatsNormalized c (displS c ++ bonusS c)
    pieceT = piece.head
    setP = extractor setA
    offP = extractor offA
    -- Logic to create variant layers
    off p = filter ((/=p).pieceT) setP ++ filter ((==p).pieceT) offP
    variants = setP : map (sortOn pieceT . off . pieceT) offP
    -- FIX: This helper supplies the empty Build [] to start recursion for each variant
    processVariant acc layers = foldRecursivelyS callback baseSL acc layers []

-- 3. The main function
fold4pcBuilds :: Character -> ((Build,Statline) -> a -> a) -> a -> [(Stat,Double)] -> Int -> [Artifact] -> [Artifact] -> a
fold4pcBuilds c callback initialAcc rollW n =
    foldOffpieceBuildsS (bestPieces rollW n) c callback initialAcc

best4pcBuilds :: [(Stat,Double)]->Int->[Artifact]->[Artifact]->[Build]
best4pcBuilds rollW n = offpieceBuilds (bestPieces rollW n)

bestBuilds :: [(Stat,Double)]->Int->[Artifact] -> [[Artifact]]
bestBuilds rollW n = sequence.bestPieces rollW n

pieceNumMlt :: Array Piece Double
pieceNumMlt = array (Flower,Circlet)
  [(Flower,1.4),(Plume,1.4),(Sands,1),(Goblet,0.5),(Circlet,1)]

pieceAwareExtractor :: [(Stat, Double)] -> Int -> [Artifact] -> [[Artifact]]
pieceAwareExtractor rollW depth = map (takeBest.sortByVal).groupSortOn piece where
  sw = map statValueToRoll rollW
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

maxDamage :: Character->[Build]->Double
maxDamage c = maximum.map (dmgClc c [])

calcStatWeights :: Character -> [Build] -> [(Stat, Double)] -> [(Stat, Double)]
calcStatWeights crctr builds = map updateW where
    maxDmg buff = maximum.map bdc $ builds where bdc = dmgClc crctr buff
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

-- Build complete statlines from builds
buildStatlines :: Character -> [Build] -> [Statline]
buildStatlines c builds = map toStatline builds where
  charStats = collectStatsNormalized c (displS c ++ bonusS c)
  toStatline b = addStatlines charStats buildStats
    where buildStats = collectStatsNormalized c (concatMap stats b)

-- Apply single-stat buff to statline (buff in rolls, converted to value)
buffStatline :: Statline -> Stat -> Double -> Statline
buffStatline sl s rolls = appendStats sl [statRollToValue (s, rolls)]

-- Calculate sensitivity: damage change per roll of stat
-- Uses ±17 rolls (≈2 good rolls) to measure slope
calcSensitivity :: (Statline -> Double) -> [Statline] -> Stat -> Double
calcSensitivity dmgCalc statlines s = (plusDmg - minusDmg) / baseDmg * 100 / 4 where
  baseDmg = maximum (map dmgCalc statlines)
  buffed rolls = map (\sl -> dmgCalc (buffStatline sl s rolls)) statlines
  plusDmg = maximum (buffed 17)
  minusDmg = maximum (buffed (-17))

constraintSlope :: Character -> [Statline] -> (Stat,Double) -> (Stat,Double)
constraintSlope c statlines (cs,cv) = (cs, if dmg minRD == dmg maxRD then cv else dDmg/dAvgRolls) where
  -- Extract min/max value of stat cs across all builds
  getStat sl = statAccessor sl cs
  minS = minimumBy (comparing getStat) statlines
  maxS = maximumBy (comparing getStat) statlines
  (minR,maxR) = constraintRange (getStat minS) (getStat maxS) cv

  -- Find best build at each range boundary
  dmg = stDmgClc c
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
calcStatWeightsC :: Character->[Build]->[(Stat,Double)]->[(Stat,Double)]
calcStatWeightsC c builds = map updateW where
  allStatlines = buildStatlines c builds
  validStatlines = filter (conditionChecker c) allStatlines
  -- Use valid builds if any, else all (enables weight calc even when no builds meet constraints)
  statlines = if null validStatlines then allStatlines else validStatlines
  dmgCalc = stDmgClc c
  updateW (s, oldW)
    | null cnd = (s, calcSensitivity dmgCalc statlines s)
    | otherwise = constraintSlope c allStatlines (head cnd)
    where cnd = filter ((==s).fst) (condition c)

-- Balanced weights updater - simple sensitivity analysis
calcStatWeightsB :: Character->[Build]->[(Stat,Double)]->[(Stat,Double)]
calcStatWeightsB c builds = map updateW where
  dmgCalc sl = if conditionChecker c sl then stDmgClc c sl else 0
  statlines = buildStatlines c builds
  updateW (s, _) = (s, calcSensitivity dmgCalc statlines s)

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

-- The state we carry through the fold
data OptState = OptState
  { bestB :: !Build           -- The absolute best build so far
  , maxD  :: !Double          -- Damage of bestB
  , maxSens :: ![(Stat, Double)] -- Max damage found for (+Stat) scenarios
  , minSens :: ![(Stat, Double)] -- Max damage found for (-Stat) scenarios
  }

-- Helper to create an empty state
initialOptState :: Character -> OptState
initialOptState c = OptState [] 0.0 zeroes zeroes
  where zeroes = map (\s -> (s, 0.0)) (scaling c)

bestBuildFolding :: Int -> Character -> [Artifact] -> [Artifact] -> Build
bestBuildFolding n c setA offA = bb
  where
    -- 1. Setup constants and helpers
    calc sla = if conditionChecker c sla then stDmgClc c sla else 0
    scaleStats = scaling c

    -- This is the callback run at every single leaf (Build)
    -- It updates the Global Max and the Sensitivity Maxes
    optimizer :: (Build, Statline) -> OptState -> OptState
    optimizer (b, sl) (OptState bb md maxS minS) =
        OptState newBB newMD newMaxS newMinS
      where
        dmg = calc sl

        -- Update Best Build
        (newBB, newMD) = if dmg > md then (b, dmg) else (bb, md)

        -- Check Sensitivity: 
        -- What if this build had +17 (approx 2 rolls) of Stat X?
        -- We track the theoretical MAX damage available for that scenario.
        updateSens (s, currentMax) = (s, max currentMax (calc (dec s 17)))
        updateSensMin (s, currentMax) = (s, max currentMax (calc (dec s (-17))))

        -- Decorator helper: temporarily add val to stat s
        dec s val = appendStats sl [statRollToValue (s, val)]

        newMaxS = map updateSens maxS
        newMinS = map updateSensMin minS

    -- 2. The Loop
    go :: OptState -> [(Stat, Double)] -> Build
    go oldState oldWeights =
        if newMax > oldMax+0.001 -- Stop if improvement is negligible
        then go newState newWeights
        else bestB oldState
      where
        oldMax = maxD oldState

        -- RUN THE FOLD (The heavy lifting happens here)
        newState = fold4pcBuilds c optimizer (initialOptState c) (extendWeights c oldWeights) n setA offA
        newMax = maxD newState

        -- Calculate new weights based on the data we just collected
        -- Formula: (MaxWithPlus - MaxWithMinus) / BaseMax
        newWeights = zipWith calcW (maxSens newState) (minSens newState)
        calcW (s, plusDmg) (_, minusDmg) =
             (s, (plusDmg - minusDmg) / newMax * 25) -- *25 is arbitrary scaling factor

    -- 3. Kickoff
    startWeights = zip scaleStats [1,1..]
    bb = go (initialOptState c) startWeights
-- NEW API WRAPPERS (delegate to CharacterBuildInfo)
-- These use the bug-fixed, optimized ArtifactInfo-based implementation
bestBuildNew :: Int -> Character -> [Artifact] -> [Artifact] -> Build
bestBuildNew n c setA offA = map aiOriginal buildInfo
  where
    setInfos = map (toArtifactInfo c) setA
    offInfos = map (toArtifactInfo c) offA
    buildInfo = bestBuildInfo n c setInfos offInfos

bestBuildFoldingNew :: Int -> Character -> [Artifact] -> [Artifact] -> Build
bestBuildFoldingNew n c setA offA = map aiOriginal buildInfo
  where
    setInfos = map (toArtifactInfo c) setA
    offInfos = map (toArtifactInfo c) offA
    buildInfo = bestBuildFoldingInfo n c setInfos offInfos

module CharacterBuildInfo where

import Artifact
import Character
import Statline
import Core.Utils
import Data.Ord (comparing, Down(..))
import Data.Array
import Data.List (maximumBy, minimumBy, foldl', sortOn)
import Data.List.Extra (groupSortOn)

-- CORE TYPES
type ArtifactStorage = ([Artifact], [Artifact])

-- SCORING (O(1) with Weightline)
scoreArtifactInfo :: Weightline -> ArtifactInfo -> Double
scoreArtifactInfo wl ai = weightStatline wl (aiStatline ai)
{-# INLINE scoreArtifactInfo #-}

-- BEST PIECES SELECTION
bestPiecesInfo :: Weightline -> Int -> [ArtifactInfo] -> [[ArtifactInfo]]
bestPiecesInfo wl n = map takeBestN . partition where
  partition = filter (not.null) . elems . partitionOnPiece aiPiece
  takeBestN = take n . sortOn (Down . scoreArtifactInfo wl)

pieceNumMlt :: Array Piece Double
pieceNumMlt = array (Flower,Circlet)
  [(Flower,1.4),(Plume,1.4),(Sands,1),(Goblet,0.5),(Circlet,1)]

pieceAwareExtractorInfo :: Weightline -> Int -> [ArtifactInfo] -> [[ArtifactInfo]]
pieceAwareExtractorInfo wl depth = map (takeBest . sortByVal) . groupSortOn aiPiece where
  sortByVal = sortOn (Down . scoreArtifactInfo wl)
  takeBest l = take n l where
    p = aiPiece (head l)
    n = round (fromIntegral depth * pieceNumMlt!p)

type BuildComponents = [[ArtifactInfo]]  -- list of piece groups for one build variant

prepareComponentSets 
  :: ([ArtifactInfo] -> [[ArtifactInfo]])  -- extractor (groups and sorts by piece)
  -> [ArtifactInfo]  -- on-set artifacts
  -> [ArtifactInfo]  -- off-set artifacts  
  -> [BuildComponents]
prepareComponentSets extractor setA offA = variants
  where
    pieceT = aiPiece . head
    setP = extractor setA
    offP = extractor offA
    off p = filter ((/=p) . pieceT) setP ++ filter ((==p) . pieceT) offP
    variants = setP : map (sortOn pieceT . off . pieceT) offP

-- BUILD GENERATION
offpieceBuildsInfo :: ([ArtifactInfo] -> [[ArtifactInfo]]) -> [ArtifactInfo] -> [ArtifactInfo] -> [BuildInfo]
offpieceBuildsInfo extractor setA offA = 
  concatMap sequence (prepareComponentSets extractor setA offA)

best4pcBuildsInfo :: Weightline -> Int -> [ArtifactInfo] -> [ArtifactInfo] -> [BuildInfo]
best4pcBuildsInfo wl n = offpieceBuildsInfo (bestPiecesInfo wl n)

-- FOLD INFRASTRUCTURE
-- Recursive helper accumulating Statline from ArtifactInfo
-- sl: accumulated Statline (char base + artifacts so far)
-- currentStack: BuildInfo being constructed
foldRecursivelySInfo :: ((BuildInfo, Statline) -> a -> a) -> Statline -> a -> [[ArtifactInfo]] -> BuildInfo -> a
foldRecursivelySInfo f sl acc [] currentStack = f (currentStack, sl) acc
foldRecursivelySInfo f sl acc (candidates:rest) currentStack =
  foldl' processCandidate acc candidates
  where
    processCandidate acc' candidate =
      -- Accumulate normalized artifact stats (aiStatline already normalized)
      let newSL = addStatlines sl (aiStatline candidate)
      in foldRecursivelySInfo f newSL acc' rest (candidate:currentStack)

-- Generic fold over all 4pc builds using ArtifactInfo
foldOffpieceBuildsInfo :: ([ArtifactInfo] -> [[ArtifactInfo]]) -> Character -> ((BuildInfo, Statline) -> a -> a) -> a -> [ArtifactInfo] -> [ArtifactInfo] -> a
foldOffpieceBuildsInfo extractor c callback initialAcc setA offA =
    foldl' processVariant initialAcc (prepareComponentSets extractor setA offA)
  where
    baseSL = collectStatsNormalized c (displS c ++ bonusS c)
    processVariant acc layers = foldRecursivelySInfo callback baseSL acc layers []

-- Main fold function using ArtifactInfo and Weightline
fold4pcBuildsInfo :: Character -> ((BuildInfo, Statline) -> a -> a) -> a -> Weightline -> Int -> [ArtifactInfo] -> [ArtifactInfo] -> a
fold4pcBuildsInfo c callback initialAcc wl n =
    foldOffpieceBuildsInfo (bestPiecesInfo wl n) c callback initialAcc

-- Traversal infrastructure
traverseBuildComponents 
  :: Character
  -> ((BuildInfo, Double), [Statline])
  -> BuildComponents 
  -> ((BuildInfo, Double), [Statline])
traverseBuildComponents char (bestSoFar, statlinesSoFar) components = 
  go baseSL components [] bestSoFar statlinesSoFar
  where
    baseSL = collectStatsNormalized char (displS char ++ bonusS char)
    scoreF = statlineDamageCalculator char
    
    go :: Statline -> [[ArtifactInfo]] -> [ArtifactInfo] 
       -> (BuildInfo, Double) -> [Statline] 
       -> ((BuildInfo, Double), [Statline])
    go accSL [] accArts best@(bestBuild, bestScore) statlines = 
      let score = scoreF accSL
          newBest = if score > bestScore 
                    then (reverse accArts, score)
                    else best
      in (newBest, accSL : statlines)
    
    go accSL (pieceGroup:rest) accArts bestSoFar statlines =
      foldl' processPiece (bestSoFar, statlines) pieceGroup
      where
        processPiece (currBest, currSLs) art =
          go (addStatlines accSL (aiStatline art)) rest (art:accArts) currBest currSLs

best4pcStatlines :: Character -> Weightline -> Int -> [ArtifactInfo] -> [ArtifactInfo] 
                     -> ((BuildInfo, Double), [Statline])
best4pcStatlines char wl n setA offA =
  foldl' (traverseBuildComponents char) (([], -1), []) componentSets
  where
    componentSets = prepareComponentSets (bestPiecesInfo wl n) setA offA

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

--reverses order of elements while partitioning
partitionOnPieceR :: (a -> Piece) -> [a] -> Array Piece [a]
partitionOnPieceR f = accumArray (flip (:)) [] (Flower,Circlet) . map (\x -> (f x, x))
--preserves order of elements
partitionOnPiece :: (a -> Piece) -> [a] -> Array Piece [a]
partitionOnPiece f = partitionOnPieceR f . reverse

-- PARETO FILTERING
-- Pareto filtering using ArtifactInfo (direct Statline comparison, no Array allocation)
-- Returns: (forward-only filtered, full pareto frontier)
-- forward-only: preserves input order, artifacts not dominated when first seen
-- full pareto: maintains pareto optimality, order undefined
paretoFilterBothInfo :: Character -> [ArtifactInfo] -> ([ArtifactInfo], [ArtifactInfo])
paretoFilterBothInfo c = go [] []
  where
    scl = scaling c
    -- filtered: current pareto frontier (fully optimal)
    -- forward: artifacts that passed forward-only filter (input order)
    go filtered forward [] = (reverse forward, reverse filtered)
    go filtered forward (a:rest)
      | isDominated = go filtered forward rest
      | otherwise = go newFiltered (a:forward) rest
      where
        slA = aiStatline a
        isDominated = any (\f -> dominates (aiStatline f) slA) filtered
        newFiltered = a : filter (not . dominates slA . aiStatline) filtered
        -- f dominates a if f >= a on all scaling stats
        dominates slF slA' = all (\s -> statAccessor slF s >= statAccessor slA' s) scl

-- Forward-only filter: partition by piece, apply paretoBoth, extract forward-only
paretoFilterInfo :: Character -> [ArtifactInfo] -> [ArtifactInfo]
paretoFilterInfo c = concatMap filterPiece . groupSortOn aiPiece
  where
    filterPiece infos = fst (paretoFilterBothInfo c infos)

-- Full pareto filter: partition by piece, apply paretoBoth, extract pareto frontier
paretoFilterRealInfo :: Character -> [ArtifactInfo] -> [ArtifactInfo]
paretoFilterRealInfo c = concatMap filterPiece . groupSortOn aiPiece
  where
    filterPiece infos = snd (paretoFilterBothInfo c infos)

-- MAIN OPTIMIZATION ENTRY POINTS
-- bestBuildInfo: iterative weight refinement
bestBuildInfo :: Int -> Character -> [ArtifactInfo] -> [ArtifactInfo] -> BuildInfo
bestBuildInfo n c setA offA = bb where
  bmkr w = best4pcStatlines c w n setA offA
  rollW = zip (scaling c) [1,1..]
  wl = defaultWeightline c
  firsts = bmkr wl
  (bb,_) = go firsts rollW
  go ((oldBest, oldMax), oldBuilds) oldRollW
    | newMax > oldMax = go news newRollW
    | oldMax < 1 = ([], oldRollW)
    | otherwise = (oldBest, oldRollW)
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
-- bestBuildFoldingInfo: single-pass optimization with sensitivity tracking
-- The state we carry through the fold
data OptState = OptState
  { bestB :: !BuildInfo         -- The absolute best build so far
  , maxD  :: !Double             -- Damage of bestB
  , maxSens :: ![(Stat, Double)] -- Max damage found for (+Stat) scenarios
  , minSens :: ![(Stat, Double)] -- Max damage found for (-Stat) scenarios
  }

-- Helper to create an empty state
initialOptState :: Character -> OptState
initialOptState c = OptState [] 0.0 zeroes zeroes
  where zeroes = map (\s -> (s, 0.0)) (scaling c)

bestBuildFoldingInfo :: Int -> Character -> [ArtifactInfo] -> [ArtifactInfo] -> BuildInfo
bestBuildFoldingInfo n c setA offA = bb
  where
    -- Setup constants and helpers
    calc = statlineDamageCalculator c
    scaleStats = scaling c
    -- Precompute once outside the loop
    plusBuff = makeBuffStatline scaleStats 17    -- All scaling stats +17 rolls
    minusBuff = makeBuffStatline scaleStats (-17) -- All scaling stats -17 rolls
    -- This is the callback run at every single leaf (BuildInfo)
    -- It updates the Global Max and the Sensitivity Maxes
    optimizer :: (BuildInfo, Statline) -> OptState -> OptState
    optimizer (b, sl) (OptState bb md maxS minS) =
        OptState newBB newMD newMaxS newMinS
      where
        dmg = calc sl
        -- Update Best Build
        (newBB, newMD) = if dmg > md then (b, dmg) else (bb, md)
        -- Check Sensitivity: 
        -- What if this build had +17 (approx 2 rolls) of Stat X?
        -- We track the theoretical MAX damage available for that scenario.
        updateSens (s, currentMax) = (s, max currentMax (calc (dec s plusBuff)))
        updateSensMin (s, currentMax) = (s, max currentMax (calc (dec s minusBuff)))
        -- Decorator helper: temporarily add val to stat s
        dec s bs = appendStats sl [(s, statAccessor bs s)]
        newMaxS = map updateSens maxS
        newMinS = map updateSensMin minS
    -- The Loop
    go :: OptState -> [(Stat, Double)] -> BuildInfo
    go oldState oldRollW =
        if newMax > oldMax+0.001 -- Stop if improvement is negligible
        then go newState newRollW
        else bestB oldState
      where
        oldMax = maxD oldState
        -- RUN THE FOLD (The heavy lifting happens here)
        newWL = rollsToWeightline oldRollW
        newState = fold4pcBuildsInfo c optimizer (initialOptState c) newWL n setA offA
        newMax = maxD newState
        -- Calculate new weights based on the data we just collected
        -- Formula: (MaxWithPlus - MaxWithMinus) / BaseMax
        newRollW = zipWith calcW (maxSens newState) (minSens newState)
        calcW (s, plusDmg) (_, minusDmg) =
             (s, (plusDmg - minusDmg) / newMax * 25) -- *25 is arbitrary scaling factor
    -- Kickoff
    startWeights = zip scaleStats [1,1..]
    bb = go (initialOptState c) startWeights

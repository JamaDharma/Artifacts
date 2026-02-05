module Main where

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

printResult :: String -> Bool -> IO()
printResult testName result = putStrLn output where
    output = "Test " ++ testName ++ ": " ++ showResult
    showResult = if result then "Passed" else "Failed"

main :: IO ()
main = do
  putStrLn "Running tests..."
  results <- sequence testSuite
  putStrLn$ "Tests run:"++show (length results)
  putStrLn$ "Tests passed:"++show (length (filter (==True) results))
  putStrLn "finished."

testSuite :: [IO Bool]
testSuite = [
              --checkPartitionig,
              --checkProcessData,
              --checkStatistics,
              --checkPareto,

              --testMinimisation
              --testMyFurina, testMyNefer
              --testUpgradeSimulator
              --foldingBestBuilds
              measureProgression
              --testWeightProgression
              --measureAndRecordX
              --compareX
              --demonstrateX
              --,testFurinaInForest
            ]

generateArts :: Int -> IO ([Artifact], [Artifact])
generateArts n = withDeterministicRandom n $ do
  setArts <- generateArtifacts "GT" 10000
  offArts <- generateArtifacts "MS" 10000
  return (setArts, offArts)

--13.8s 13s 12.7s 10s 7s 6.5s 7.7s
measureProgression :: IO Bool
measureProgression = do
    (setArts, offArts) <- generateArts 1
    (t, prg) <- whileMeasuringTime $ do
      let prg = progression furina (bestBuildFolding 7) setArts offArts
      --let prg = progression furina (bestBuild 7) setArts offArts
      putStrLn$ "AllBuilds: "++show (length prg)
      return prg
    print.map fst $ prg
    putStrLn.unwords.map (printf "%6.1f").analitics (dmgClc furina [])$prg
    print t
    return True

topBuilds :: Character -> Int -> [Build] -> [Double]
topBuilds chr cnt = take cnt.reverse.sort.map (dmgClc chr [])
foldingBestBuilds :: IO Bool
foldingBestBuilds = do
    setArts <- generateArtifacts "GT" 10000
    offArts <- generateArtifacts "MS" 10000
    let ttb = topBuilds furina 10
    let ecsldc sl = if asr ER < 200 then 0 else stDmgClc furina asr where asr = statAccessor sl
    let rollW = extendWeights furina.zip (scaling furina)$[1,1..]
    let bldNrm = best4pcBuilds rollW 7 setArts offArts
    let acc (b,s) a = b:a
    let foldBlld = fold4pcBuilds furina acc [] rollW 7 setArts offArts
    let vld (b,s) a = a && (abs (dmgClc furina [] b - ecsldc s) < 0.1)
    let valid = fold4pcBuilds furina vld True rollW 7 setArts offArts
    print $ length bldNrm
    print (ttb bldNrm)
    print $ length foldBlld
    print (ttb foldBlld)

    return valid

-- In Tests.hs, add this function:
testUpgradeSimulator :: IO Bool
testUpgradeSimulator = go nefer 1000000 where
  pairsToDays :: Int -> Double
  pairsToDays n = fromIntegral n*40/180 --180 resin per day, 40 resin per artifact pair
  printInfo :: Double -> (Piece, Int) -> IO ()
  printInfo int (p,c) = putStrLn $ show p ++ ": " ++ show c ++ " (" ++ printf "%.0f" (int / fromIntegral (c+1)) ++ " days)"
  go chr cnt= do
    let interval = pairsToDays cnt
    putStrLn ("Loading "++name chr++" build...")
    artifacts <- readGOODForCharacter "data/Main_2026-02-03_11-27-42.json" (name chr)
    putStrLn $ "Loaded " ++ show (length artifacts) ++ " artifacts"

    results <- simulateUpgrades chr artifacts cnt

    putStrLn "\nUpgrade frequency:"

    mapM_ (printInfo interval) results
    return True

checkPartitionig :: IO Bool
checkPartitionig = do
  let lst = [(Flower,1),(Flower,2),(Plume,7),(Plume,6),(Plume,4)]
  let res = partitionOnPiece fst lst
  let resp = map snd (res!Plume)
  --print resp
  return (resp == [7,6,4])

checkProcessData :: IO Bool
checkProcessData = do
  let table = [
        (0,[(1, 10.0), (2, 20.0)]),
        (1,[(1, 15.0), (2, 25.0)])
        ]
  let expected = [(1,array (0,1) [(0,(1,10.0)),(1,(1,15.0))]),(2,array (0,1) [(0,(2,20.0)),(1,(2,25.0))])]
  let actual = processData table (listArray (0,1) [(0,0),(0,0)])
  --print actual
  return (actual == expected)

checkStatistics :: IO Bool
checkStatistics = do
  let table = [
        [(1, [10.0]), (3, [20.0])],
        [(1, [15.0]), (2, [25.0])]
        ]

  let expected = [[(1,[10.0]),(3,[20.0])],[(1,[15.0]),(2,[25.0])]]
  let actual = statistics' head [0,1] table
  --print actual
  return (actual == expected)

checkPareto :: IO Bool
checkPareto = do
  (setArts, offArts) <- generateArts 10
  let prfF = paretoFilter furina
  let prfR = paretoFilterReal furina

  let (sF,oF) = (prfF setArts, prfF offArts)
  let afFL@(sFL,oFL) = (length sF, length oF)
  let (sR,oR) = (prfR setArts, prfR offArts)
  let afRL@(sRL,oRL) = (length sR, length oR)

  --putStrLn$"False: "++ show afFL
  --putStrLn$"Real: "++ show afRL

  return (sRL<=sFL && oRL<=oFL)

testMinimisation :: IO Bool
testMinimisation = do
  let itr b = foldl (flip add) b c where
        dr1 = map (firstD b) scaling
        dr2 = secondD b dr1
        c = calcChanges (dmg bestBuild0) dr1 dr2 b
  print bestBuild0
  print (getBuildInfo bestBuild0)
  let first = map (firstD bestBuild0) scaling
  print first
  let second = secondD bestBuild0 first
  print second
  putStrLn "Calculating changes:"
  let chg = calcChanges (dmg bestBuild0) first second bestBuild0
  print chg
  let buildsP = take 20$iterate itr bestBuild0
  let printBuild (i,b) = do
        print i
        print b
        print (getBuildInfo b)
        let first = map (firstD b) scaling
        print first
        let second = secondD b first
        print second
        print$ calcChanges (dmg bestBuild0) first second b
  mapM_ printBuild (zip [1..] buildsP)

  return True  where

    base = [(HP, 70), (CR, 45), (CD, 50), (DMG, 100), (ER, 200)]
    weights = [(HP, 1), (CR, 2), (CD, 1), (DMG, 1), (ER, 1)]
    --scaling = map fst weights
    scaling = [HP,CR,CD]
    bestBuild0 = [(HP, 150), (CR, 85), (CD, 195), (DMG, 101), (ER, 210)]

    getV s = snd.head.filter ((==s).fst)
    evalW :: [(Stat, Double)] -> Double
    evalW = sum.map sw where sw (s,v) = v*getV s weights
    dmg statLine = (1+sl HP)*(1+sl DMG)*(1+sl CR* sl CD)*100
      where sl s = (/100).getV s$statLine
    getBuildInfo ::[(Stat,Double)] -> (Double, Double)
    getBuildInfo b = (dmg b, evalW b)
    add (s,v) = map modify where
      modify ce@(cs,cv)
        | cs == s = (cs, cv + v)
        | otherwise = ce
    step = 0.1
    firstD b s = (s,drv (makeP (-step)) (makeP step)) where
      makeP dx = (dmg$ add (s,dx) b, dx)
      drv (dmg1,dx1) (dmg2,dx2) = (dmg2-dmg1) / (dx2-dx1)
    secondD b fd = go goodD where
      goodD = filter ((/=0).snd) fd
      go [p1,p2] = [drv2 p1 p2, drv2 p2 p1]
      go (p1:p2:t) = drv2 p1 p2 : go (p2:t)
      go _ = []
      drv2 (s1,d1) (s2, d2) = (s1,dd1) where
        step2 = step*d1/d2
        pnt2 = add (s1,-step).add (s2,step2) $ b
        d12 = snd.firstD pnt2 $ s1
        dd1 = (d12-d1)/(-step)
    calcOtherChanges dr dr2 sa sb = (cnst,mlt) where
      wba2b = getV sb weights / getV sa weights / getV sb dr2
      mlt = getV sa dr2 * wba2b
      cnst = getV sa dr * wba2b - getV sb dr / getV sb dr2
    calcOtherSub dr dr2 sa sb = (drb*c,drb*m) where
      drb = getV sb dr
      (c,m) = calcOtherChanges dr dr2 sa sb
    substitution dr dr2 (h:t) = (h, (0,getV h dr):map (calcOtherSub dr dr2 h) t)
    solutionA (sa, lst) dc = (sa, (dc - sum cl)/sum ml) where
      (cl,ml) = unzip lst

    calcChanges dmg0 dr dr2 b = solA:res where
      dmgCorrection = dmg0 - dmg b
      subs = substitution dr dr2 scaling
      solA = solutionA subs dmgCorrection
      oc = calcOtherChanges dr dr2 (fst subs)
      calcChange s = (s,change $ oc s)
        where change (c,m) = snd solA*m+c
      res = map calcChange.filter (/=fst subs)$scaling

--(f'a+f''a*da)/wa=(f'b+f''b*db)/wb
--(f'a+f''a*da)*wb/wa=(f'b+f''b*db)
--db=((f'a+f''a*da)*wb/wa - f'b)/f''b
--db=f'a/f''b*wb/wa+f''a/f''b*wb/wa*da - f'b/f''b




getMeasure :: Int -> Int -> IO (Double, [(Int, Double)])
getMeasure depth cases = do
    let stW = zip (scaling furina) [1,1..]
    let damage = dmgClc furina []
    let bm = best4pcBuilds stW depth
    let dfs = damageFromSeed damage 10000 bm
    let getResults = go where
        go [] = return []
        go (seed:t) = do
              putStr ("\r"++show seed++"      ")
              dmg <- dfs seed
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
    let prefix = "simple"
    let reporter = uncurry (getReport prefix)
    reports <- mapM reporter [(5,150),(10,50),(15,10)]
    writeFile ("data/"++prefix++"Report.hs") (unlines reports)
    return True

demonstrateX :: IO Bool
demonstrateX = do
  (setArts, offArts) <- generateArts 10
  let strategy2 = BuildStrategy {
              character = furina,
              buildMaker = \(s,o) w -> best4pcBuilds (extendWeights furina w) 7 s o,
              weightCalculator = calcStatWeightsC furina
            }
  let prf = paretoFilterReal furina
  let b1 = bestBuildStrategic strategy2 (setArts, offArts)
  let fsA = prf setArts
  let fsO = prf offArts
  let b2 = bestBuildStrategic strategy2 (fsA,fsO)

  print (length setArts, length offArts)
  putStrLn$"Build 1: "++ show (dmgClc furina [] b1)
  mapM_ print b1
  print (length fsA, length fsO)
  print (length (paretoFilter furina setArts), length (paretoFilter furina offArts))
  putStrLn$"Build 2: "++ show (dmgClc furina [] b2)
  mapM_ print b2


  return True
--[10,18,38,63,76,90,96], 38 is failure for updateWeightsC.
--[18,38,49,63,76,90,92,96], pareto failure is 10?!!.
--[18,38,47,60,76,90,92,96] both paretoed, why?!!
--[18,38,63,76,90,96] with fixed filter even worst of all..
compareX :: IO Bool
compareX = do
    putStrLn "Comparing X"
    let damage = dmgClc furina []
    let bm1 s o = [bestBuild 7 furina s o]
    let strategy2 = BuildStrategy {
                  character = furina,
                  buildMaker = \(s,o) w -> best4pcBuilds (extendWeights furina w) 7 s o,
                  weightCalculator = calcStatWeightsC furina
                }
    let prf = paretoFilterReal furina
    let bm2 s o = [bestBuildStrategic strategy2 (prf s,prf o)]
    let dfs = buildMakerDiff damage 10000 bm1 bm2
    let getResults = go where
        go [] = return []
        go (seed:t) = do
              diff <- dfs seed
              if diff /= 0
                then putStrLn ("Seed: " ++ show seed ++ " Diff: " ++ show diff)
                else do
                  putStr (show seed++", ")
                  hFlush stdout
              rest <- go t
              return ( (seed,diff):rest)
    r <- getResults [0..100]
    putStrLn$"\nResults:"++show (map fst.filter ((/=0).snd)$r)
    let differences = map snd r
    let count condition = length (filter condition differences)
    print [ ("Less than 0", count (< 0))
          , ("Equal to 0", count (== 0))
          , ("Greater than 0", count (> 0))
          ]
    return (count (< 0) == 0)

testMyFurina :: IO Bool
testMyFurina = do
    furinaA <- readGOOD "data/furina.json"
    let dmg = dmgClc furina [] furinaA
    print dmg
    let result = abs (dmg - 28129)<1
    printResult "My Furina Damage" result
    return result

testMyNefer :: IO Bool
testMyNefer = do
    neferA <- readGOODForCharacter "data/Main_2026-02-03_11-27-42.json" "Nefer"
    let dmg = dmgClc nefer [] neferA
    print dmg
    let result = abs (dmg - 28129)<1
    printResult "My Nefer Damage" result
    return result

--fail 10, 46 fails 15 too
furinaFailureSeeds = [46,98,113,194,241,299,323,351,449,496]

testFurinaInForest :: IO Bool
testFurinaInForest = do
    furinaA <- readGOOD "data/furina.json"
    let dmg = dmgClc furina [] furinaA
    let splitArts = partition ((=="GoldenTroupe").set) furinaA
    let stW = zip (scaling furina) [1,1..]
    let pf  =  paretoFilter furina
    let bm = best4pcBuilds stW 10
    let bm2 s o = let (_,_,b) = updateWeights furina 8 stW s o in b
    let bm3 s o = [bestBuild 7 furina s o]
    --let bm2 sp op = bm (pf sp) (pf op)

    let dfs = buildsFromSeed (dmgClc furina []) (fst splitArts) (snd splitArts) 10000 bm3
    let failureSeeds = go 0 0 where
        go count seed
          | count >= 10 = return []
          | otherwise = do
              sDmg <- dfs seed
              if sDmg < dmg
                then do
                  print ("Seed: " ++ show seed ++ " Damage: " ++ show sDmg)
                  rest <- go (count + 1) (seed+1)
                  return (seed:rest)
                else go count (seed+1)

    dmgs <- mapM dfs furinaFailureSeeds
    print dmgs
    print $ take 10.filter ((<dmg).snd).zip [1..] $ dmgs

    --fs <- failureSeeds
    --print fs
    return True

testWeightProgression :: IO Bool
testWeightProgression = do
  (setArts, offArts) <- generateArts 38
  --let rollW = zip (scaling furina) [2,2..]
  let rollW = zip (scaling furina) [1,2..]
  let dmg = dmgClc furina []
  let goldBuild = bestBuild 7 furina setArts offArts
  mapM_ print goldBuild
  putStrLn "WeightProgression gold:"
  print (dmg goldBuild)
  let maxDmg = maximum.map dmg
  let cnd a
        | p == Circlet && ms /= CD  = False
        | p == Sands && ms /= HP  = False
        | p == Goblet && ms /= HP  = False
        | otherwise = True
        where
          p = piece a
          ms = fst.head.stats$a
  let fltSet = filter cnd setArts
  let fltOff = filter cnd offArts
  let bmkr w = best4pcBuilds (extendWeights furina w) 3 fltSet fltOff

  let go (_,oldSW) = (newMax,newSW) where
        newBuilds = bmkr oldSW
        newSW = calcStatWeightsC furina newBuilds oldSW
        newMax = maxDmg newBuilds
  let res = take 20$iterate go (0, rollW)
  putStrLn.unlines.map show$res
  return True

module BuildSearchComparision
    (measureAndRecordX,testBuildMakerRegression) where

import TestData
import Control.Exception (evaluate)
import ArtifactType ( Artifact, Build )
import Data.List.Extra ( group, sort, groupSortOn )
import Character ( Character(dmgClc), furina )
import CharacterBuild
    ( bestBuild,bestBuildFolding, paretoFilterReal )
import GeneratorUtils ( artifactsFromSeed, whileMeasuringTime )
import System.IO (hFlush, stdout)

type BuildFinder = Int -> [Artifact] -> [Artifact] -> Build

-- | Character used for all regression tests - hardcoded to ensure consistency
-- with recorded benchmark data in TestData.hs
regTestChr :: Character
regTestChr = furina

getMeasure :: BuildFinder -> Int -> Int -> IO (Double, [(Int, Double)])
getMeasure bf depth cases = do
    let damage = dmgClc regTestChr []
    let getResults = go where
        go [] = return []
        go (seed:seeds) = do
            putStr ("\r" ++ show seed ++ "      ")
            hFlush stdout
            (setA, offA) <- artifactsFromSeed seed
            let build = bf depth setA offA
                dmg = damage build
            _ <- evaluate dmg
            rest <- go seeds
            return $ (seed, dmg) : rest
    whileMeasuringTime (getResults [0..cases-1])

getReport :: String -> BuildFinder -> Int -> Int -> IO String
getReport prefix bf depth cases = do
    res <- getMeasure bf depth cases
    let report = prefix ++ "_" ++ show depth ++ "P_10K_" ++ show cases ++ "S = " ++ show res
    return report

measureAndRecordX :: IO Bool
measureAndRecordX = do
    let prefix = "bestBuild"
    let bf depth = bestBuild depth regTestChr
    reports <- mapM (uncurry $ getReport prefix bf) [(5,150),(10,50),(15,10)]
    writeFile ("data/" ++ prefix ++ "Report.hs") (unlines reports)
    return True

-- | Find minimum depth where build finder achieves target damage for a seed
-- Returns (-1, diff) if failed to match target danage even at maxDepth
findMinDepth :: BuildFinder -> Int -> Double -> Int -> IO (Int, Double)
findMinDepth bf seed targetDamage maxDepth = do
    (setA, offA) <- artifactsFromSeed seed
    let tryDepth depth
            | depth > maxDepth = (-1, 0) --should never happen
            | diff + 0.1 > 0 = (depth, diff)
            | depth == maxDepth = (-1, diff)
            | otherwise = tryDepth (depth + 1)
          where
            damage = dmgClc regTestChr []
            diff = damage (bf depth setA offA) - targetDamage
    return $ tryDepth 1

printRegressionTable :: String -> [(Int, Int, Double)] -> IO ()
printRegressionTable suiteName results = do
    putStrLn $ "\nSuite: " ++ suiteName
    putStrLn "Depth | Count | Seeds"
    putStrLn "----------------------"
    let indexed = map formatResult results
        grouped = groupSortOn snd indexed
        groupToRow g = (snd $ head g, length g, map fst g)
    mapM_ (printRow . groupToRow) grouped
  where
    formatResult (seed, depth, diff)
        | diff >= 0.1 = (seed, show depth ++ "*")
        | otherwise = (seed, show depth)
    showPadded str = str ++ replicate (6 - length str) ' '
    printRow (depthStr, count, seeds) =
        putStrLn $ showPadded depthStr ++ "| " ++ showPadded (show count) ++ "| " ++ showSeeds seeds
    showSeeds ss
        | length ss <= 5 = show ss
        | otherwise = show (take 3 ss) ++ "..." ++ show (reverse $ take 2 $ reverse ss)

testBuildMakerRegression :: IO Bool
testBuildMakerRegression = do
    let pf = paretoFilterReal regTestChr
    let bf depth = bestBuild depth regTestChr
    --let bf d set off = bestBuild d regTestChr (pf set) (pf off)
        maxDepth = 15
        fmd (seed,target) = do
            (dp,df) <- findMinDepth bf seed target maxDepth
            _ <- evaluate dp
            _ <- evaluate df
            putStr ("\rProcessed seed: " ++ show seed ++ " Depth: " ++ show dp ++ "    ")
            hFlush stdout
            return (seed, dp, df)
        runSuite name (_,refData) = do
            results <- mapM fmd refData
            printRegressionTable name results
            return $ all (\(_, d, _) -> d /= -1) results
    pass1 <- runSuite "BestBuild 5P (150 cases)" bestBuild_5P_10K_150S
    pass2 <- runSuite "BestBuild 10P (50 cases)" bestBuild_10P_10K_50S
    pass3 <- runSuite "BestBuild 15P (10 cases)" bestBuild_15P_10K_10S
    return $ pass1 && pass2 && pass3
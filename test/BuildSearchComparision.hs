module BuildSearchComparision where

import TestData
import Control.Exception (evaluate)
import ArtifactType ( Artifact, Build )
import Data.List.Extra ( group, sort )
import Character ( Character(dmgClc), furina )
import CharacterBuild
    ( bestBuild )
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

printRegressionTable :: String -> [(Int, Double)] -> IO ()
printRegressionTable suiteName results = do
    putStrLn $ "\nSuite: " ++ suiteName
    putStrLn "Depth | Count"
    putStrLn "--------------"
    let formatted = map formatResult results
        grouped = group $ sort formatted
        rows = map (\g -> (head g, length g)) grouped
    mapM_ printRow rows
  where
    formatResult (depth, diff)
        | diff >= 0.1 = show depth ++ "*"
        | otherwise = show depth
    printRow (depthStr, count) = 
        putStrLn $ depthStr ++ replicate (6 - length depthStr) ' ' ++ "| " ++ show count

testBuildMakerRegression :: IO Bool
testBuildMakerRegression = do
    let bf depth = bestBuild depth regTestChr
        maxDepth = 15
        runSuite name (_,refData) = do
            results <- mapM (\(seed, target) -> findMinDepth bf seed target maxDepth) refData
            printRegressionTable name results
            return $ all (\(d, _) -> d /= -1) results
    pass1 <- runSuite "BestBuild 5P (150 cases)" bestBuild_5P_10K_150S
    pass2 <- runSuite "BestBuild 10P (50 cases)" bestBuild_10P_10K_50S
    pass3 <- runSuite "BestBuild 15P (10 cases)" bestBuild_15P_10K_10S
    return $ pass1 && pass2 && pass3
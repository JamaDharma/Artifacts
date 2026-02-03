module Generator (
    generateArtifacts,
    generateArtifactForPiece
    ) where

import ArtifactType
import Weights
import Data.List (group,sort)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Array

randomElement :: [a] -> IO a
randomElement xs = do
    i <- randomRIO (0, length xs - 1)
    return (xs !! i)

selectWeightedItem :: [(a, Double)] -> IO a
selectWeightedItem [] = error "Empty list provided to selectWeightedItem"
selectWeightedItem [(item, _)] = return item  -- No randomization needed for single item
selectWeightedItem items = do
    let cumulativeWeights = scanl1 (+) (map snd items)
        totalWeight = last cumulativeWeights
    randVal <- randomRIO (0, totalWeight)
    return $ fst $ head $ dropWhile ((< randVal) . snd) (zip (map fst items) cumulativeWeights)


filterOut :: Eq a => a -> [(a, b)] -> [(a, b)]
filterOut k = filter notK where notK (key,_) = key /= k

-- Helper function to select unique substats
selectUniqueSubstats :: Int -> [(Stat, Double)] -> IO [Stat]
selectUniqueSubstats 0 _ = return []
selectUniqueSubstats n pool = do
    substat <- selectWeightedItem pool
    let remainingPool = filterOut substat pool
    rest <- selectUniqueSubstats (n - 1) remainingPool
    return (substat : rest)

--Function simulating substat rolling
rollSubstats :: Int->[Stat] -> IO [(Stat,Double)]
rollSubstats n sts = do
    randomRolls <- replicateM n $ randomElement sts
    let allRolls = map (\x -> (head x, length x)).group.sort$(sts++randomRolls)
    mapM roll allRolls


--Function simulating one stat rolling
roll:: (Stat,Int) -> IO (Stat,Double)
roll (s,n) = do
    rolls <- replicateM n (randomElement [7..10])
    return (s,sum rolls)


-- Internal function that takes piece weights as parameter
generateArtifactInternal :: String -> [(Piece, Double)] -> IO Artifact
generateArtifactInternal sn pieceWeights = do
    p <- selectWeightedItem pieceWeights
    let statWeights = pieceMSW ! p
    mainStat <- selectWeightedItem statWeights
    substats <- selectUniqueSubstats 4 (filterOut mainStat substatWeight)
    sUpgarades <- selectWeightedItem substatUpgradeNumW
    substatsWithRolls <- rollSubstats sUpgarades substats
    let statLine = (mainStatValue mainStat:).map statRollToValue$substatsWithRolls
    return Artifact{piece=p,set=sn,upNumber=sUpgarades,stats=statLine}

-- Original function - generates random piece
generateArtifact :: String -> IO Artifact
generateArtifact sn = generateArtifactInternal sn pieceWeight

-- New function - generates specific piece
generateArtifactForPiece :: String -> Piece -> IO Artifact
generateArtifactForPiece sn p = generateArtifactInternal sn [(p, 1.0)]

generateArtifacts :: String->Int->IO [Artifact]
generateArtifacts setName n = replicateM n  (generateArtifact setName)
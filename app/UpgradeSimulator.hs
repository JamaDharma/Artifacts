module UpgradeSimulator (
    simulateUpgrades
    ) where

import Artifact
import Character
import Generator (generateArtifactForPiece)
import Data.List (group, sort)
import Control.Monad (replicateM)

-- Test N artifacts for a specific piece, count how many are upgrades
testPieceUpgrades :: Character -> Build -> Piece -> Int -> IO Int
testPieceUpgrades char currentBuild pieceType count = do
    let oldDamage = dmgClc char [] currentBuild
    let otherPieces = filter ((/= pieceType) . piece) currentBuild
    let isUpgrade newArt = dmgClc char [] (newArt : otherPieces) > oldDamage
    
    -- Set name "unused" because damage formula doesn't check set bonuses
    artifacts <- replicateM count (generateArtifactForPiece "unused" pieceType)
    return $ length $ filter isUpgrade artifacts

-- Top level: simulate upgrade frequency for current build
simulateUpgrades :: Character -> Build -> Int -> IO [(Piece, Int)]
simulateUpgrades char currentBuild totalCount = do
    results <- mapM testPiece currentBuild
    return results
  where
    buildSets = group . sort $ map set currentBuild
    mainSet = head $ head $ filter largeEnough buildSets
    largeEnough g = length g >= 4
    
    isOnSet art = set art == mainSet
    
    -- totalCount counts artifact pairs (on-set + off-set generated together)
    -- offSetCount is double because both on-set and off-set pieces can go in off-piece slot
    artCount art = if isOnSet art then onSetCount else offSetCount
    onSetCount = totalCount `div` 5
    offSetCount = (totalCount * 2) `div` 5
    
    testPiece art = do
        count <- testPieceUpgrades char currentBuild (piece art) (artCount art)
        return (piece art, count)
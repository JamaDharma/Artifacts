{-# LANGUAGE TupleSections #-}
module Core.Progression where

import Artifact
import Character
import Core.Pareto
import Core.Utils (aiStatline, ArtifactInfo(..), BuildInfo, toArtifactInfo)
import Data.Array (elems, Array, (//), listArray)
import Data.List
import Data.Ord (comparing, Down (Down))
import Data.List.Extra

analitics::(Build->Double)->[(Int,Build)]->[Double]
analitics clc prg = zipWith dmgPerIndex prg (tail prg) where
  dmgPerIndex (i1,b1) (i2,b2) = (clc b2 - clc b1)/fromIntegral (i2-i1)*3500/clc b1

statistics :: (Build -> Double) -> [Int] -> [[(Int, Build)]] -> [[(Int, Build)]]
statistics = statistics'

statistics' :: Eq a => ([a] -> Double) -> [Int] -> [[(Int, [a])]] -> [[(Int, [a])]]
statistics' clc pts tbl = map (map head.groupOn snd) combinedProgressions
  where
    combinedProgressions = transpose.map processSlice.processData (zip [0..] tbl)$ acc
      where acc = listArray (0, length tbl - 1) (replicate (length tbl) (0,[]))
    -- Extracts points of interests from each slice of data
    processSlice (index, arr) = map (index,) points where
      sorted = sortOn clc.map snd.elems$arr
      points = map (sorted!!) pts

--transposes sparse data
processData :: [(Int,[(Int, a)])] -> Array Int (Int, a) -> [(Int,Array Int (Int, a))]
processData [] _ = []
processData currentData currentAcc = (currentIndex,updatedAcc): processData updatedData updatedAcc
  where
    currentIndex = minimum $ map (fst.head.snd) currentData
    (accUpdate, dataUpdate) = mapAccumL consumeIndex [] currentData
    updatedAcc = currentAcc // accUpdate
    updatedData = filter (not.null.snd) dataUpdate
    consumeIndex accUpd (prgNum,sublist) = (accUpd++map (prgNum,) u,(prgNum,l))
      where (u,l) = break ((>currentIndex).fst) sublist

-- Internal worker: processes ArtifactInfo throughout
progressionInfo :: Character
                -> (Character -> [ArtifactInfo] -> [ArtifactInfo] -> BuildInfo)
                -> [(Int,ArtifactInfo)] -> [(Int,ArtifactInfo)]
                -> [(Int,BuildInfo)]
progressionInfo c bb reversedIndexedArtifacts1 reversedIndexedArtifacts2 =
  updateBestBuild reversedIndexedArtifacts1 reversedIndexedArtifacts2 []
  where
    updateBestBuild :: [(Int,ArtifactInfo)] -> [(Int,ArtifactInfo)] -> [(Int,BuildInfo)] -> [(Int,BuildInfo)]
    updateBestBuild remaining1 remaining2 acc =
      let
        currentArtifacts1 = map snd remaining1
        currentArtifacts2 = map snd remaining2
        newBestBuild = bb c currentArtifacts1 currentArtifacts2
      in
        findProgression remaining1 remaining2 newBestBuild acc

    findProgression :: [(Int,ArtifactInfo)] -> [(Int,ArtifactInfo)] -> BuildInfo -> [(Int,BuildInfo)] -> [(Int,BuildInfo)]
    findProgression [] [] _ acc = acc
    findProgression remaining1 [] cbb acc =
      consume (tail remaining1) [] cbb acc (head remaining1)
    findProgression [] remaining2 cbb acc =
      consume [] (tail remaining2) cbb acc (head remaining2)
    findProgression rem1@((idx1,art1):rest1) rem2@((idx2,art2):rest2) cbb acc
      | idx2 > idx1 = consume rem1 rest2 cbb acc (idx2,art2)
      | otherwise   = consume rest1 rem2 cbb acc (idx1,art1)

    consume :: [(Int,ArtifactInfo)] -> [(Int,ArtifactInfo)] -> BuildInfo -> [(Int,BuildInfo)] -> (Int,ArtifactInfo) -> [(Int,BuildInfo)]
    consume rem1 rem2 currentBestBuild acc (idx, art)
      | idx < 1 = updatedAcc
      | art `elem` currentBestBuild = updateBestBuild rem1 rem2 updatedAcc
      | otherwise = findProgression rem1 rem2 currentBestBuild acc
      where updatedAcc = (idx, currentBestBuild):acc

-- Public wrapper: handles Artifact ↔ ArtifactInfo conversion
progression :: Character
            -> (Character -> [ArtifactInfo] -> [ArtifactInfo] -> BuildInfo)
            -> [Artifact] -> [Artifact]
            -> [(Int,Build)]
progression c bb artifacts1 artifacts2 =
  map convertResult results
  where
    convertResult (idx, buildInfo) = (idx, map aiOriginal buildInfo)
    
    processArtifacts :: [Artifact] -> [(Int,ArtifactInfo)]
    processArtifacts arts = reverseSorted . paretoFiltered $ indexed
      where
        indexed = zip [0..] (map (toArtifactInfo c) arts)
        paretoFiltered = concatMap filterPiece . elems . partitionOnPiece (aiPiece . snd)
        filterPiece pairs = fst (paretoBothOn c (aiStatline . snd) pairs)
        reverseSorted = sortBy (comparing (Down . fst))

    reversedIndexedArtifacts1 = processArtifacts artifacts1
    reversedIndexedArtifacts2 = processArtifacts artifacts2
    
    results = progressionInfo c bb reversedIndexedArtifacts1 reversedIndexedArtifacts2
{-# LANGUAGE TupleSections #-}
module Progression where

import ArtifactType
import CharacterBuild
import Character (Character)
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

progression :: Character->(Character->[Artifact]->[Artifact]->Build)->[Artifact]->[Artifact]->[(Int,Build)]
progression c bb artifacts1 artifacts2 =
  let
    processArtifacts::[Artifact]->[(Int,Artifact)]
    processArtifacts = reverseSorted.paretoFiltered.indexed where
      indexed = zip [0..]
      paretoFiltered = map (paretoCandidatesOn c snd).elems.partitionOnPiece (piece.snd)
      reverseSorted = (sortBy (comparing (Data.Ord.Down . fst)) . concat)

    reversedIndexedArtifacts1 = processArtifacts artifacts1
    reversedIndexedArtifacts2 = processArtifacts artifacts2
  in
    updateBestBuild reversedIndexedArtifacts1 reversedIndexedArtifacts2 []
  where
    updateBestBuild :: [(Int,Artifact)] -> [(Int,Artifact)] -> [(Int,Build)] -> [(Int,Build)]
    updateBestBuild remaining1 remaining2 acc =
      let
        currentArtifacts1 = map snd remaining1
        currentArtifacts2 = map snd remaining2
        newBestBuild = bb c currentArtifacts1 currentArtifacts2
      in
        findProgression remaining1 remaining2 newBestBuild acc

    findProgression :: [(Int,Artifact)] -> [(Int,Artifact)] -> Build -> [(Int,Build)] -> [(Int,Build)]
    findProgression [] [] _ acc = acc

    findProgression remaining1 [] cbb acc =
      consume (tail remaining1) [] cbb acc (head remaining1)

    findProgression [] remaining2 cbb acc =
      consume [] (tail remaining2) cbb acc (head remaining2)

    findProgression rem1@((idx1,art1):rest1) rem2@((idx2,art2):rest2) cbb acc
      | idx2 > idx1 = consume rem1 rest2 cbb acc (idx2,art2)
      | otherwise   = consume rest1 rem2 cbb acc (idx1,art1)

    consume :: [(Int,Artifact)] -> [(Int,Artifact)] -> Build -> [(Int,Build)] -> (Int,Artifact) -> [(Int,Build)]
    consume rem1 rem2 currentBestBuild acc (idx, art)
      | idx < 1 = updatedAcc
      | art `elem` currentBestBuild = updateBestBuild rem1 rem2 updatedAcc
      | otherwise = findProgression rem1 rem2 currentBestBuild acc
      where updatedAcc = (idx, currentBestBuild):acc
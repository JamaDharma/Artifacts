module Core.Traversal where

import Statline
import Character
import Core.Utils

import Data.List ( foldl', sortOn )
import Core.Pareto (partitionOnPiece)
import Data.Ord ( Down(Down) )
import Data.Array ( elems )

scoreArtifactInfo :: Weightline -> ArtifactInfo -> Double
scoreArtifactInfo wl ai = weightStatline wl (aiStatline ai)
{-# INLINE scoreArtifactInfo #-}

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
                    then (reverse accArts, score)--why reverse? Order doesn't matter.. Check if removing reverse improves performabce
                    else best
      in (newBest, accSL : statlines)
    
    go accSL (pieceGroup:rest) accArts bestSoFar statlines =
      foldl' processPiece (bestSoFar, statlines) pieceGroup
      where
        processPiece (currBest, currSLs) art =
          go (addStatlines accSL (aiStatline art)) rest (art:accArts) currBest currSLs

bestPiecesInfo :: Weightline -> Int -> [ArtifactInfo] -> [[ArtifactInfo]]
bestPiecesInfo wl n = map takeBestN . partition where
  partition = filter (not.null) . elems . partitionOnPiece aiPiece
  takeBestN = take n . sortOn (Down . scoreArtifactInfo wl)

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

best4pcStatlines :: Character -> Weightline -> Int -> [ArtifactInfo] -> [ArtifactInfo] 
                     -> ((BuildInfo, Double), [Statline])
best4pcStatlines char wl n setA offA =
  foldl' (traverseBuildComponents char) (([], -1), []) componentSets
  where
    componentSets = prepareComponentSets (bestPiecesInfo wl n) setA offA

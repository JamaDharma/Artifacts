module Core.Pareto where

import Artifact
import Character
import Statline
import Core.Utils (ArtifactInfo(..))
import Data.List.Extra (groupSortOn)
import Data.Array (Array, accumArray)

--partitioning
--reverses order of elements while partitioning
partitionOnPieceR :: (a -> Piece) -> [a] -> Array Piece [a]
partitionOnPieceR f = accumArray (flip (:)) [] (Flower,Circlet) . map (\x -> (f x, x))
--preserves order of elements
partitionOnPiece :: (a -> Piece) -> [a] -> Array Piece [a]
partitionOnPiece f = partitionOnPieceR f . reverse

-- ============================================================================
--  MODERN ARTIFACT INFO IMPLEMENTATION
--  (Optimized, uses Statline accessors and ArtifactInfo)
-- ============================================================================

-- | Core logic: Filters a list of ArtifactInfo.
-- Returns a tuple: (Forward-Filtered List, Full Pareto Frontier)
-- 1. Forward-Filtered: Preserves input order; keeps artifacts not dominated by those seen *before* them.
-- 2. Pareto Frontier: The subset of artifacts that are strictly optimal (not dominated by *any* other).
paretoBothOn :: Character -> (a->Statline) -> [a] -> ([a], [a])
paretoBothOn c getStatline = go [] []
  where
    scl = scaling c
    -- filtered: current pareto frontier (fully optimal)
    -- forward: artifacts that passed forward-only filter (input order)
    go filtered forward [] = (reverse forward, reverse filtered)
    go filtered forward (a:rest)
      | isDominated = go filtered forward rest
      | otherwise = go newFiltered (a:forward) rest
      where
        slA = getStatline a
        -- Check if 'a' is dominated by anything currently in the frontier
        isDominated = any (\f -> dominates (getStatline f) slA) filtered
        -- Remove things from the frontier that are now dominated by 'a'
        newFiltered = a : filter (not . dominates slA . getStatline) filtered
        -- f dominates a if f >= a on all scaling stats
        dominates slF slA' = all (\s -> statAccessor slF s >= statAccessor slA' s) scl

paretoFilterBothInfo :: Character -> [ArtifactInfo] -> ([ArtifactInfo], [ArtifactInfo])
paretoFilterBothInfo c = paretoBothOn c aiStatline

-- | Filters artifacts by piece, keeping those that are candidates for the best build.
-- Uses Forward-Filtered logic (preserves some suboptimal artifacts if they appeared early, usually for stable sorting).
paretoFilterInfo :: Character -> [ArtifactInfo] -> [ArtifactInfo]
paretoFilterInfo c = concatMap filterPiece . groupSortOn aiPiece
  where
    filterPiece infos = fst (paretoFilterBothInfo c infos)

-- | Filters artifacts by piece, returning only the strict Pareto Frontier.
paretoFilterRealInfo :: Character -> [ArtifactInfo] -> [ArtifactInfo]
paretoFilterRealInfo c = concatMap filterPiece . groupSortOn aiPiece
  where
    filterPiece infos = snd (paretoFilterBothInfo c infos)
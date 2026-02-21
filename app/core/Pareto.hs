module Core.Pareto where

import Artifact
import Character
import Statline
import Core.Utils (ArtifactInfo(..))
import Data.List.Extra (groupSortOn)
import Data.Array

--partitioning

data PiecePool a = PiecePool
  { ppOnSet  :: Array Piece [a]
  , ppOffSet :: Array Piece [a]
  } deriving (Show)

-- | Apply f to every piece group in both arrays
mapPool :: ([a] -> [b]) -> PiecePool a -> PiecePool b
mapPool f (PiecePool on off) = PiecePool (fmapArr f on) (fmapArr f off)
  where fmapArr g arr = listArray (bounds arr) (map g (elems arr))

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

-- | INTERNAL: One-pass pareto accumulation without reversal.
-- Takes pre-accumulated filtered/forward lists and processes remaining items.
-- Returns (forward, filtered) in REVERSE order - caller must reverse.
-- DO NOT USE DIRECTLY unless you understand the accumulation order.
paretoUnsafeAccum :: [Stat] -> (a->Statline) -> [a] -> [a] -> [a] -> ([a], [a])
paretoUnsafeAccum scl getStatline filtered forward = go
  where
    go [] = (forward, filtered)
    go (a:rest)
      | isDominated = go rest
      | otherwise = go' rest
      where
        slA = getStatline a
        isDominated = any (\f -> dominates (getStatline f) slA) filtered
        newFiltered = a : filter (not . dominates slA . getStatline) filtered
        go' = paretoUnsafeAccum scl getStatline newFiltered (a:forward)
        dominates slF slA' = all (\s -> statAccessor slF s >= statAccessor slA' s) scl

    
paretoOnPiece :: Character -> (a->ArtifactInfo) -> [a] -> [a] -> [a] -> ([a], [a])
paretoOnPiece char getAI flt fwd items = (fwdResult, fullResult)
  where
    paretoRaw = paretoUnsafeAccum (scaling char) (aiStatline . getAI)
    fltByPiece = partitionOnPiece (aiPiece . getAI) flt
    fwdByPiece = partitionOnPiece (aiPiece . getAI) fwd
    itemsByPiece = partitionOnPiece (aiPiece . getAI) items
    allPieces = indices itemsByPiece
    results = [paretoRaw (lookup p fltByPiece) (lookup p fwdByPiece) (itemsByPiece ! p)
              | p <- allPieces]
    lookup p arr = if inRange (bounds arr) p then arr ! p else []
    fwdResult = concatMap fst results
    fullResult = concatMap snd results

-- | Core logic: Filters a list of ArtifactInfo.
-- Returns a tuple: (Forward-Filtered List, Full Pareto Frontier)
-- 1. Forward-Filtered: Preserves input order; keeps artifacts not dominated by those seen *before* them.
-- 2. Pareto Frontier: The subset of artifacts that are strictly optimal (not dominated by *any* other).
paretoBothOn :: Character -> (a->Statline) -> [a] -> ([a], [a])
paretoBothOn c getStatline items = (reverse forward, reverse filtered)
  where
    scl = scaling c
    (forward, filtered) = paretoUnsafeAccum scl getStatline [] [] items

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
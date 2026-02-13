module Core.Utils
  ( ArtifactInfo(..)
  , BuildInfo
  , toArtifactInfo
  , collectStatsNormalized
  , defaultWeightline
  , rollsToWeightline
  ) where


import Artifact
import Character
import Statline 
import Data.List (foldl')
import Data.Array ( (!) )

--structure for high performance core
data ArtifactInfo = ArtifactInfo
  { aiPiece :: !Piece
  , aiMainStat :: !Stat  -- first element of stats
  , aiStatline :: !Statline  -- normalized stats (artifact contribution only)
  , aiOriginal :: Artifact  -- for export/display
  } deriving (Show)

type BuildInfo = [ArtifactInfo]

toArtifactInfo :: Character -> Artifact -> ArtifactInfo
toArtifactInfo c art = ArtifactInfo
  { aiPiece = piece art
  , aiMainStat = fst (head (stats art))
  , aiStatline = collectStatsNormalized c (stats art)
  , aiOriginal = art
  }


--When collecting from Artifact
collectStatsNormalized :: Character -> [(Stat, Double)] -> Statline
collectStatsNormalized c statList = foldl' addStat zeroStatline normalized
  where
    normalized = concatMap normalize statList
    bs = baseS c
    hasBase s = s `elem` scaling c
    flatToP s v
        | hasBase s = [(s, v / (bs ! s) * 100)]
        | otherwise = []
    normalize (s, v)
      | s == HPf  = flatToP HP v
      | s == ATKf = flatToP ATK v
      | s == DEFf = flatToP DEF v
      | s == DMGb = []  -- drop it
      | otherwise = [(s, v)]
    addStat sl (stat, val) = case stat of
      HP  -> sl { slHP  = slHP sl + val }
      ATK -> sl { slATK = slATK sl + val }
      DEF -> sl { slDEF = slDEF sl + val }
      ER  -> sl { slER  = slER sl + val }
      EM  -> sl { slEM  = slEM sl + val }
      CR  -> sl { slCR  = slCR sl + val }
      CD  -> sl { slCD  = slCD sl + val }
      HB  -> sl { slHB  = slHB sl + val }
      DMG -> sl { slDMG = slDMG sl + val }
      _   -> sl  -- HPf/ATKf/DEFf/DMGb ignored

-- Convert roll weights to value Weightline (for core optimization)
-- Core assumes stats are already normalized (no HPf/ATKf/DEFf)
rollsToWeightline :: [(Stat, Double)] -> Weightline
rollsToWeightline = foldl' addWeight zeroStatline
  where
    -- statValueToRoll on weights converts per-roll to per-value weights
    -- We're changing 1/roll to 1/value, so usual conversion meaning is inverted
    toValueW = statValueToRoll

    addWeight wl (stat, rollW) =
      let (_, valueW) = toValueW (stat, rollW)
      in case stat of
        HP  -> wl { slHP  = slHP wl + valueW }
        ATK -> wl { slATK = slATK wl + valueW }
        DEF -> wl { slDEF = slDEF wl + valueW }
        ER  -> wl { slER  = slER wl + valueW }
        EM  -> wl { slEM  = slEM wl + valueW }
        CR  -> wl { slCR  = slCR wl + valueW }
        CD  -> wl { slCD  = slCD wl + valueW }
        HB  -> wl { slHB  = slHB wl + valueW }
        DMG -> wl { slDMG = slDMG wl + valueW }
        HPf  -> error "Flat stats should not appear in core weights"
        ATKf -> error "Flat stats should not appear in core weights"
        DEFf -> error "Flat stats should not appear in core weights"
        DMGb -> error "DMGb not used in weights"

-- Default starting weights (1.0 per roll for each scaling stat)
defaultWeightline :: Character -> Weightline
defaultWeightline c = rollsToWeightline (zip (scaling c) (repeat 1.0))
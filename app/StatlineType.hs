module StatlineType where

import Data.Array ( bounds, inRange, (!) )
import ArtifactType
import Data.List (foldl')

data Statline = Statline
  { slHP  :: !Double
  , slATK :: !Double
  , slDEF :: !Double
  , slER  :: !Double
  , slEM  :: !Double
  , slCR  :: !Double
  , slCD  :: !Double
  , slHB  :: !Double
  , slDMG :: !Double
  } deriving (Show, Eq)

{-# INLINE statAccessor #-}
statAccessor :: Statline -> Stat -> Double
statAccessor sl stat = case stat of
  HP  -> slHP sl
  ATK -> slATK sl
  DEF -> slDEF sl
  ER  -> slER sl
  EM  -> slEM sl
  CR  -> slCR sl
  CD  -> slCD sl
  HB  -> slHB sl
  DMG -> slDMG sl
  HPf  -> error "Flat stats should be normalized before Statline creation"
  ATKf -> error "Flat stats should be normalized before Statline creation"
  DEFf -> error "Flat stats should be normalized before Statline creation"
  DMGb -> error "DMGb is not used"

{-# INLINE zeroStatline #-}
zeroStatline :: Statline
zeroStatline = Statline 0 0 0 0 0 0 0 0 0

{-# INLINE appendStats #-}
appendStats :: Statline -> [(Stat, Double)] -> Statline
appendStats = foldl' addStat
  where
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
      _   -> sl
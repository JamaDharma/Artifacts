module Statline where

import Artifact
import Data.List (foldl')

data Statline = Statline
  { slHP  :: {-# UNPACK #-} !Double
  , slATK :: {-# UNPACK #-} !Double
  , slDEF :: {-# UNPACK #-} !Double
  , slER  :: {-# UNPACK #-} !Double
  , slEM  :: {-# UNPACK #-} !Double
  , slCR  :: {-# UNPACK #-} !Double
  , slCD  :: {-# UNPACK #-} !Double
  , slHB  :: {-# UNPACK #-} !Double
  , slDMG :: {-# UNPACK #-} !Double
  } deriving (Show, Eq, Ord)

type Weightline = Statline

-- Core scoring operation: dot product of weights and stats
-- Replaces artValue list iteration with O(1) field access
{-# INLINE weightStatline #-}
weightStatline :: Weightline -> Statline -> Double
weightStatline wl sl = 
  slHP wl * slHP sl + slATK wl * slATK sl + slDEF wl * slDEF sl +
  slER wl * slER sl + slEM wl * slEM sl + slCR wl * slCR sl +
  slCD wl * slCD sl + slHB wl * slHB sl + slDMG wl * slDMG sl

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
      HPf  -> error "Flat stats should not be used as buffs"
      ATKf -> error "Flat stats should not be used as buffs"
      DEFf -> error "Flat stats should not be used as buffs"
      DMGb -> error "DMGb is not used"

addStatlines :: Statline -> Statline -> Statline
addStatlines sl1 sl2 = Statline
  { slHP  = slHP sl1 + slHP sl2
  , slATK = slATK sl1 + slATK sl2
  , slDEF = slDEF sl1 + slDEF sl2
  , slER  = slER sl1 + slER sl2
  , slEM  = slEM sl1 + slEM sl2
  , slCR  = slCR sl1 + slCR sl2
  , slCD  = slCD sl1 + slCD sl2
  , slHB  = slHB sl1 + slHB sl2
  , slDMG = slDMG sl1 + slDMG sl2
  }
module Character(
  Character(..),
  furina,
  getShownStat,
  getFinalStatline,
  getScalingStatline,
  conditionChecker,
  Statline, statAccessor, statDecorator, collectStats, appendStats, nefer
) where

import ArtifactType
import Data.Array
import qualified Data.Array.Unboxed as U

data Character = Character
  { name     :: String
  , scaling  :: [Stat]
  , baseS    :: Array Stat Double--base values flat HP ATK DEF
  , displS   :: [(Stat, Double)]--values
  , bonusS   :: [(Stat, Double)]--values
  , condition:: [(Stat, Double)]--minimum values
  , dmgClc   :: [(Stat,Double)]->Build->Double
  , stDmgClc :: (Stat->Double)->Double
  }
--Statline for efficient access to agregate character stats
type Statline = U.UArray Stat Double
statAccessor :: Statline -> Stat -> Double
statAccessor = (U.!)
statDecorator :: Statline -> (Stat, Double) -> Stat -> Double
statDecorator sl (bs,bv) s --accessor decorated with single buff
  | s == bs = sl U.!s + bv
  | otherwise = sl U.!s
collectStats :: [(Stat, Double)] -> Statline
collectStats = U.accumArray (+) 0.0 (HPf, DMGb)
appendStats :: Statline -> [(Stat, Double)] -> Statline
appendStats = U.accum (+)

-- Combine stats into an Array Stat Double, summing values and using 0.0 as default
combineStats :: [(Stat, Double)] -> Array Stat Double
combineStats = accumArray (+) 0.0 (HPf, DMGb)
addStats :: Array Stat Double -> [(Stat, Double)] -> Array Stat Double
addStats = accum (+)

getShowStatline :: Character->Build->Array Stat Double
getShowStatline c = combineStats.concat.(displS c:).map stats

getFinalStatline :: Character->Build->Array Stat Double
getFinalStatline c = combineStats.concat.(charStats:).map stats
  where charStats = displS c ++ bonusS c

getShownStat::Character->Build->Stat->Double
getShownStat c arts s
  | inRange (HP,DEF) s = statline!(flatMap!s) + (baseS c)!s * (1+statline!s/100)
  | inRange (ER,DMG) s = statline!s
  | otherwise = error ("Invalid stat" ++ show s)
  where statline = getShowStatline c arts

simpleMult :: (Stat->Double) -> Double
simpleMult sl = critMlt*dmgMlt where
  eCR = max 0 (min 100 (sl CR))/100
  critMlt = 1 + eCR*sl CD/100
  dmgMlt = 1 + sl DMG/100

getScalingStatline :: Character->Artifact->Array Stat Double
getScalingStatline c a = combineStats (map fIncS.scaling $ c) where
  fIncS s
    | inRange (HP,DEF) s = (s, sl!s + sl!(flatMap!s)/(baseS c)!s*100)
    | otherwise = (s,sl!s)
  sl = combineStats (stats a)

conditionChecker :: Character -> (Stat -> Double) -> Bool
conditionChecker c sla = all (\(cs,cv)->sla cs >= cv).condition$c

characterDamageCalculator :: Character->[(Stat,Double)]->(Build->Double)
characterDamageCalculator c buff = buildDmg where
  sdc = stDmgClc c
  buffStatline = combineStats (displS c ++ bonusS c ++ buff)
  buildDmg build = if conditionChecker c sla then sdc sla else 0
    where sla = ((addStats buffStatline.concatMap stats$build)!)

furina :: Character
furina = Character{
   name = "Furina",
   scaling = [ER,HP,CR,CD,DMG],
   baseS  = baseStats [(HP,15307)],
   --baseS  = baseStats [(HP,14553)],
   displS = [(HP,25),(ER,100+45.9),(CR,24.2),(CD,50)],
   bonusS = [(CR,16),(DMG,70+75+28)],--28 is depndent on her hp but whatever
   condition = [(ER,200)],
   dmgClc = characterDamageCalculator furina,
   stDmgClc = furinaStatlineDmgClc furina
}

furinaStatlineDmgClc :: Character->((Stat->Double)->Double)
furinaStatlineDmgClc c = innerDmg where
  baseHP = baseS c!HP
  defMlt = 190/390
  baseMV = 14.92/100*1.4*0.9*defMlt
  innerDmg sl = dmgOutput  where
    effHp = sl HPf+baseHP*(1+sl HP/100)
    dmgOutput = effHp*baseMV*simpleMult sl

nefer :: Character
nefer = Character{
   name = "Nefer",
   scaling = [EM, CR, CD],
   baseS  = baseStats [],  -- no base stats needed (EM/CR/CD don't use them)
   displS = [(EM, 100+80), (CR, 5+30), (CD, 50+38.4+55.1)],
   -- 100 char EM, 80 artifact set (2pc), 5% base CR, 30% artifact set (4pc), 
   -- 50% base CD, 38.4% char, 55.1% weapon
   bonusS = [(EM, 100+450), (CR, 10), (CD, 20)],
   -- 100 Nefer talent (conditional), 450 team EM, 
   -- 10% Lunar-Bloom CR, 20% Lunar-Bloom CD
   condition = [],  -- no stat requirements
   dmgClc = characterDamageCalculator nefer,
   stDmgClc = neferStatlineDmgClc nefer
}

neferStatlineDmgClc :: Character -> ((Stat->Double)->Double)
neferStatlineDmgClc _ = innerDmg where
  talentMV = 1.728*(1+3*0.08) -- with bonus
  lunarBloomDmgBonus = 0.746  -- 74.6% from team
  lunarBloomBaseMult = 1+0.35   -- 35% from team
  flatDmgIncrease = 5030.0    -- from Lauma (roughly constant, depends on Lauma stats)
  
  innerDmg sl = finalDmg where
    totalEM = sl EM
    transformativeBonus = 6 * totalEM / (totalEM + 2000)
    reactionMult = 1 + transformativeBonus + lunarBloomDmgBonus
    
    scalingDmg = talentMV*totalEM*reactionMult*lunarBloomBaseMult
    dmgBeforeCrit = scalingDmg + flatDmgIncrease
    
    critMult = simpleMult sl  -- reuse Furina's crit multiplier function
    finalDmg = dmgBeforeCrit * critMult
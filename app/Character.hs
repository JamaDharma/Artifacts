module Character(
  Character(..),
  furina,nefer,
  getShownStat,
  getFinalStatline,
  getScalingStatline,
  conditionChecker,
  collectStatsNormalized
) where

import ArtifactType
import StatlineType
import Data.Array
import Data.List (foldl')

data Character = Character
  { name     :: String
  , scaling  :: [Stat]
  , baseS    :: Array Stat Double--base values flat HP ATK DEF
  , displS   :: [(Stat, Double)]--values
  , bonusS   :: [(Stat, Double)]--values
  , condition:: [(Stat, Double)]--minimum values
  , dmgClc   :: [(Stat,Double)]->Build->Double
  , stDmgClc :: Statline->Double
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

critMult :: Statline -> Double
critMult sl = critMlt where
  eCR = max 0 (min 100 (slCR sl))/100
  critMlt = 1 + eCR*slCD sl/100

simpleMult :: Statline -> Double
simpleMult sl = critMult sl*dmgMlt where
  dmgMlt = 1 + slDMG sl/100

getScalingStatline :: Character->Artifact->Array Stat Double
getScalingStatline c a = combineStats (map fIncS.scaling $ c) where
  fIncS s
    | inRange (HP,DEF) s = (s, sl!s + sl!(flatMap!s)/(baseS c)!s*100)
    | otherwise = (s,sl!s)
  sl = combineStats (stats a)

conditionChecker :: Character -> Statline -> Bool
conditionChecker c sl = all (\(cs,cv)->statAccessor sl cs >= cv).condition$c

characterDamageCalculator :: Character->[(Stat,Double)]->(Build->Double)
characterDamageCalculator c buff = buildDmg where
  sdc = stDmgClc c
  buildDmg build = if conditionChecker c sla then sdc sla else 0
    where sla = collectStatsNormalized c (displS c ++ bonusS c ++ buff ++ concatMap stats build)

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

furinaStatlineDmgClc :: Character->(Statline->Double)
furinaStatlineDmgClc c = innerDmg where
  baseHP = baseS c!HP
  defMlt = 190/390
  baseMV = 14.92/100*1.4*0.9*defMlt
  innerDmg sl = dmgOutput where
    effHp = baseHP*(1+slHP sl/100)  -- HPf already normalized into HP%
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

neferStatlineDmgClc :: Character -> (Statline->Double)
neferStatlineDmgClc _ = innerDmg where
  talentMV = 1.728*(1+3*0.08)
  lunarBloomDmgBonus = 0.746
  lunarBloomBaseMult = 1+0.35
  flatDmgIncrease = 5030.0
  
  innerDmg sl = finalDmg where
    totalEM = slEM sl
    transformativeBonus = 6 * totalEM / (totalEM + 2000)
    reactionMult = 1 + transformativeBonus + lunarBloomDmgBonus
    
    scalingDmg = talentMV*totalEM*reactionMult*lunarBloomBaseMult
    dmgBeforeCrit = scalingDmg + flatDmgIncrease
    
    finalDmg = dmgBeforeCrit * critMult sl
module CharacterLibrary(
  Character(..),
  ArtifactInfo(..),
  BuildInfo,
  furina,nefer,
  getShownStat,
  getFinalStatline,
  getScalingStatline,
  conditionChecker,
  statlineDamageCalculator,
  collectStatsNormalized,
  toArtifactInfo
) where

import Artifact
import Statline
import Character
import Core.Utils
import Data.Array

characterDamageCalculator :: Character->[(Stat,Double)]->(Build->Double)
characterDamageCalculator c buff = buildDmg where
  buildDmg build = statlineDamageCalculator c sla
    where sla = collectStatsNormalized c (displS c ++ bonusS c ++ buff ++ concatMap stats build)

critMult :: Statline -> Double
critMult sl = 1 + clampedCR * slCD sl * 0.0001  -- 0.0001 = 1/(100*100)
  where clampedCR = max 0 (min 100 (slCR sl))
{-# INLINE critMult #-}

simpleMult :: Statline -> Double
simpleMult sl = critMult sl*dmgMlt where
  dmgMlt = 1 + slDMG sl/100

furina :: Character
furina = Character{
   name = "Furina",
   element = HydroD,
   scaling = [ER,HP,CR,CD,DMG],
   baseS  = baseStats [(HP,15307)],
   --baseS  = baseStats [(HP,14553)],
   displS = [(HP,25),(ER,100+45.9),(CR,24.2),(CD,50)],
   bonusS = [(CR,16),(DMG,70+75+28)],--28 is depndent on her hp but whatever
   condition = [(ER,200)],
   dmgClc = characterDamageCalculator furina,
   stDmgClcUnc = furinaStatlineDmgClc furina
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
   element = DendroD,
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
   stDmgClcUnc = neferStatlineDmgClc nefer
}

neferStatlineDmgClc :: Character -> (Statline->Double)
neferStatlineDmgClc _ = innerDmg where
  talentMV = 1.728*(1+3*0.08)
  lunarBloomDmgBonus = 0.746
  lunarBloomBaseMult = 1+0.35
  flatDmgIncrease = 5030.0
  
  innerDmg sl = finalDmg where
    totalEM = slEM sl
    emFactor = totalEM / (totalEM + 2000)  -- One division
    transformativeBonus = 6 * emFactor
    reactionMult = 1 + transformativeBonus + lunarBloomDmgBonus
    
    scalingDmg = talentMV * totalEM * reactionMult * lunarBloomBaseMult
    dmgBeforeCrit = scalingDmg + flatDmgIncrease
    
    finalDmg = dmgBeforeCrit * critMult sl
  {-# INLINE innerDmg #-}  -- Or INLINE neferStatlineDmgClc
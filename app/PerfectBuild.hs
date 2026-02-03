module PerfectBuild where

import ArtifactType
import Character

firstDerivatives:: Statline->[(Stat, Double)]
firstDerivatives build weights

--slimmest build with the same damage
slimBuild :: Caracter->Statline->[(Piece,Double)]->Double
slimBuild c sl w = 1 where
    dmg = stDmgClc c
    buildDamage = dmg.statAccessor$sl
    ss = scaling c


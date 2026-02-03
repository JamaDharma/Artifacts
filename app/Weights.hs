module Weights where

import ArtifactType
import Data.Array

pieceWeight :: [(Piece, Double)]
pieceWeight = [(Flower,1),(Plume,1),(Goblet,1),(Sands,1),(Circlet,1)]

substatUpgradeNumW :: [(Int, Double)]
substatUpgradeNumW = [(4,4),(5,1)]

substatWeight :: [(Stat,Double)]
substatWeight = [
    (HPf,6),(ATKf,6),(DEFf,6),
    (HP, 4),(ATK, 4),(DEF, 4),
    (ER,4),(EM,4),(CR,3),(CD,3)
    ]

--Main stats
circletMSW :: [(Stat,Double)]
circletMSW = [(HP,22),(ATK,22),(DEF,22),(CR,10),(CD,10),(HB,10),(EM,4)]
sandsMSW :: [(Stat, Double)]
sandsMSW = [(HP,26.68),(ATK,26.66),(DEF,26.66),(ER,10.00),(EM,10.00)]
gobletMSW :: [(Stat, Double)]
gobletMSW = [(HP,19.25),(ATK,19.25),(DEF,19.25),(DMG,5),(DMGb,35),(EM,2.5)]
pieceMSW :: Array Piece [(Stat, Double)] --main stat weights
pieceMSW = array (Flower, Circlet) [
    (Flower, [(HPf,1)]), 
    (Plume, [(ATKf,1)]),
    (Goblet, gobletMSW), 
    (Sands, sandsMSW), 
    (Circlet, circletMSW)
    ]
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ImportGOOD (readGOOD,readGOODLevelled,writeGOOD,readGOODForCharacter) where

import Artifact
import RollProbability
import Data.Aeson
import GHC.Generics
import Data.ByteString.Lazy as BS (readFile, ByteString, writeFile)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Tuple (swap)


-- JSON parsing data types
data JSONGOOD = JSONGOOD
  { format    :: String
  , version   :: Int
  , artifacts :: [JSONArtifact]
  } deriving (Show, Generic)

data JSONArtifact = JSONArtifact
  { setKey     :: String
  , slotKey    :: String
  , level      :: Int
  , rarity     :: Int
  , mainStatKey:: String
  , location   :: String
  , lock       :: Bool
  , substats   :: [JSONSubStat]
  } deriving (Show, Generic)

data JSONSubStat = SubStat
  { key   :: String
  , value :: Double
  } deriving (Show, Generic)

instance FromJSON JSONGOOD
instance ToJSON JSONGOOD
instance FromJSON JSONArtifact
instance ToJSON JSONArtifact
instance FromJSON JSONSubStat
instance ToJSON JSONSubStat

-- Mapping JSON keys to Piece and Stat
data BiMap a b = BiMap { forwardMap :: Map.Map a b, backwardMap :: Map.Map b a } deriving (Show)
fromLists :: Ord a => Ord b => [(a, b)] -> BiMap a b
fromLists lst = BiMap {
  forwardMap = Map.fromList lst,
  backwardMap = Map.fromList (map swap lst)
}

pieceList :: BiMap String Piece
pieceList = fromLists
  [ ("flower", Flower)
  , ("plume", Plume)
  , ("goblet", Goblet)
  , ("sands", Sands)
  , ("circlet", Circlet)
  ]
parsePiece :: String -> Piece
parsePiece = (forwardMap pieceList Map.!)
encodePiece :: Piece -> String
encodePiece = (backwardMap pieceList Map.!)
--Terrible hack: need to implement real damage types
statList :: BiMap String Stat
statList = fromLists
  [ ("hp", HPf)
  , ("atk", ATKf)
  , ("def", DEFf)
  , ("hp_", HP)
  , ("atk_", ATK)
  , ("def_", DEF)
  , ("enerRech_", ER)
  , ("eleMas", EM)
  , ("critRate_", CR)
  , ("critDMG_", CD)
  , ("heal_", HB)
  , ("physical_dmg_", PhysD), ("anemo_dmg_", AnemoD), ("geo_dmg_", GeoD)
  , ("electro_dmg_", ElectroD), ("hydro_dmg_", HydroD), ("pyro_dmg_", PyroD)
  , ("cryo_dmg_", CryoD), ("dendro_dmg_", DendroD)
  , ("dmg_", DMG)
  , ("dmgWrong_", DMGb)
  ]
parseStat :: String -> Stat
parseStat k = case Map.lookup k (forwardMap statList) of
  Just stat -> stat
  Nothing   -> error $ "Unknown stat key: " ++ k
encodeStat :: Stat -> String
encodeStat = (backwardMap statList Map.!)

-- Convert JSONArtifact to Artifact
jsonToArtifact :: JSONArtifact -> Artifact
jsonToArtifact json = Artifact
    { piece = parsePiece (slotKey json)
    , set = setKey json
    , upNumber = round.analyzeRolls.map (round.snd.statValueToRoll)$ssts
    , stats = (mainStatValue.parseStat.mainStatKey$json) : ssts
    } where
      ssts = map substatToStat (substats json)
      substatToStat substat = (parseStat (key substat), value substat)

-- Generic function to read and filter JSON artifacts
readGOODWithFilter :: (JSONArtifact -> Bool) -> String -> IO [Artifact]
readGOODWithFilter filterFunc filePath = do
  jsonString <- BS.readFile filePath
  let good = fromJust . decode $ jsonString
  return $ map jsonToArtifact (filter filterFunc (artifacts good))

-- Parse JSON file to list of all Artifacts
readGOOD :: String -> IO [Artifact]
readGOOD = readGOODWithFilter (const True)

-- Parse JSON file to list of Artifacts with level >= 20
readGOODLevelled :: String -> IO [Artifact]
readGOODLevelled = readGOODWithFilter ((>= 20) . level)

-- Read artifacts for a specific character (all levels)
readGOODForCharacter :: String -> String -> IO [Artifact]
readGOODForCharacter filePath charName =
  readGOODWithFilter ((==charName).location) filePath

-- Convert list of Artifact to json string
encodeGOOD :: [Artifact] -> ByteString
encodeGOOD arts = encode JSONGOOD
  { format = "GOOD"
  , version = 1
  , artifacts = map artifactToJSON arts
  }
  where
    artifactToJSON artifact = JSONArtifact
      { setKey = set artifact
      , slotKey = encodePiece (piece artifact)
      , level = 20
      , rarity = 5
      , mainStatKey = encodeStat (fst (head (stats artifact)))
      , location = ""
      , lock = False
      , substats = map statToSubstat (tail (stats artifact))
      }
    statToSubstat (s, v) = SubStat{ key = encodeStat s, value = v}
-- Write list of Artifact to file as json in GOOD format
writeGOOD :: String -> [Artifact] -> IO ()
writeGOOD filePath arts = BS.writeFile filePath (encodeGOOD arts)
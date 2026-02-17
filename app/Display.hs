module Display where

import Artifact
import Text.Printf
import Data.Char (isUpper)

-- | Compact artifact display: Piece Set MainStat Substats CV
-- Format: "Flower GTDT HP%   CR:10.5 CD:21.0 ER:11.7 HP:9.3   CV:42.0"
-- Set abbreviation uses capitals if multiple, else first 4 chars
prettyPrint :: Artifact -> String
prettyPrint a =
  padN 7 (show $ piece a) ++ " " ++
  padN 4 (setAbbrev $ set a) ++ " " ++
  padN 5 (show mainStat) ++ " " ++
  formatSubstats substats ++ " " ++
  "CV:" ++ printf "%.1f" (artCV a)
  where
    (mainStat, _) = head $ stats a  -- MainStat name only, value is constant
    substats = tail $ stats a       -- 4 substats with values
    
    formatSubstats :: [(Stat, Double)] -> String
    formatSubstats = concatMap formatStat
    
    formatStat :: (Stat, Double) -> String
    formatStat (s, v) = padN 4 (show s) ++ ":" ++ printf "%.1f" v ++ " "

-- | Extract set abbreviation: 4 capitals if multiple, else first 4 chars
-- Examples: "GoldenTroupe" -> "GT", "Noblesse" -> "Nobl"
setAbbrev :: String -> String
setAbbrev s = take 4 result
  where
    capitals = filter isUpper s
    result = if length capitals > 1 then capitals else s

-- | Pad string to N characters (right padding with spaces)
padN :: Int -> String -> String
padN n s = s ++ replicate (max 0 (n - length s)) ' '

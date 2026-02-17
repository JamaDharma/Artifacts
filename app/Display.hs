module Display where

import Artifact
import Text.Printf
import Data.Char (isUpper)
import Data.List (isSuffixOf)

-- | Compact artifact display: Piece Set MainStat Substats CV
-- Format: "Flower GTDT HP%   CR:10.5 CD:21.0 ER:11.7 HP:9.3   CV:42.0"
-- Set abbreviation uses capitals if multiple, else first 4 chars
prettyPrint :: Artifact -> String
prettyPrint a =
  padN 7 (show $ piece a) ++ " " ++
  padN 4 (setAbbrev $ set a) ++ "  " ++
  padN 5 (show mainStat) ++ " " ++
  formatSubstats substats ++ " " ++
  "CV:" ++ printf "%.1f" (artCV a)
  where
    (mainStat, _) = head $ stats a  -- MainStat name only, value is constant
    substats = tail $ stats a       -- 4 substats with values
    
    formatSubstats :: [(Stat, Double)] -> String
    formatSubstats = concatMap formatStat
    
    -- | Format value: pad to 5 chars, show decimals only when needed
    -- Examples: 299.0 -> "  299", 10.5 -> "10.50", 7.0 -> "    7"
    formatValue :: Double -> String
    formatValue v
      | v >= 100 = padN 5 (show (round v :: Int))  -- 3+ digits: no decimals
      | v < 10 = padN 5 (' ':formatted)  -- line up decimals for single-digit values
      | otherwise = padN 5 formatted
      where
        -- Show 1 decimal, but strip trailing .0
        withDecimals = printf "%.1f" v
        formatted = case reverse withDecimals of
          '0':'.':rest -> reverse rest      -- Strip .0
          _ -> withDecimals                  -- Keep as is

    formatStat :: (Stat, Double) -> String
    formatStat (s, v) = 
      if isFlatStat s 
        then grayColor ++ statStr ++ ":" ++ valStr ++ resetColor ++ " "
        else statStr ++ ":" ++ valStr ++ " "
      where
        statStr = padLeft 4 (show s)
        valStr = formatValue v
        isFlatStat stat = "f" `isSuffixOf` show stat  -- HPf, ATKf, DEFf end with 'f'

-- | Extract set abbreviation: 4 capitals if multiple, else first 4 chars
-- Examples: "GoldenTroupe" -> "GT", "Noblesse" -> "Nobl"
setAbbrev :: String -> String
setAbbrev s = take 4 result
  where
    capitals = filter isUpper s
    result = if length capitals > 1 then capitals else s

-- ANSI color codes
grayColor :: String
grayColor = "\ESC[90m"  -- Dark gray
resetColor :: String
resetColor = "\ESC[0m"  -- Reset to default
-- | Pad string to N characters (right padding with spaces)
padN :: Int -> String -> String
padN n s = s ++ replicate (max 0 (n - length s)) ' '
-- | Pad string to N characters (left padding with spaces)
padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' ++ s
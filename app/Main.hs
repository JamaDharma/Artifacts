module Main where

import ArtifactType
import Character
import CharacterBuild
import Progression
import Generator
import Data.Ord (Down(..))
import Data.List.Extra (sortOn)
import Data.Array
import Text.Printf (printf)
import ImportGOOD
import ExportPlot
import Control.Monad
import Control.Concurrent.Async.Extra (mapConcurrentlyBounded)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

d2s::Double->String
d2s = printf "%6.1f"

padN :: Show a => Int->a->String
padN n x = padding++xs where
  xs = show x
  padding = replicate (n-length xs) ' '

padShow :: Show a => a->String
padShow = padN 10

prettyPrint :: Artifact -> String
prettyPrint a =
  "\t"++ padN 7 (piece a) ++"\t"++ d2s (artCV a)
    ++"\t"++ take 4 (set a)
    ++"\t ln: "++ show (upNumber a - 1)
    ++"\t RV: "++ show (artRV a * 10)
    ++"\t"++
  concatMap (\(s,v) -> "  " ++ padN 7 s ++ " " ++ d2s v ++ " ") (stats a)

weightsToString ::[(Stat, Double)] -> String
weightsToString = unwords.map w2s where
  w2s (s,w) = padN 7 s ++ printf "%6.2f" w

prntArts:: [Artifact]->IO()
prntArts = mapM_ (putStrLn.prettyPrint)

buildHeader :: (Int, Build) -> String
buildHeader (i, build) = "Build #" ++ show i ++ ": "
    ++"   СV:" ++ get buildCV
    ++"   Damage: " ++ get (dmgClc char [])
    ++ buildStatsInfo char build
  where
    get x = d2s (x build)
    buildStatsInfo c b = unwords.map statInfo $ scaling c where
      statInfo s = padN 7 s ++" "++ printf "%.1f" (getShownStat c b s)

prntBuildWithIndex :: (Int, Build) -> IO ()
prntBuildWithIndex (i, build) = do
  putStrLn$ buildHeader (i, build)
  prntArts build


-- Print all builds
prntBuilds :: [[Artifact]] -> IO ()
prntBuilds builds = mapM_ prntBuildWithIndex (zip [1..] builds)

-- Print n best builds
prntBestBuilds :: Int->[[Artifact]]->IO ()
prntBestBuilds n = prntBuilds.take n.sortOn (Data.Ord.Down. damage)

weightsIteration :: Int ->[Artifact]->[Artifact]->(Int,[(Stat, Double)])->IO (Int,[(Stat, Double)])
weightsIteration sz onA offA (n,oW) = do
  putStrLn $ "Size: " ++ show sz ++ "  Iteration: " ++ show n
  putStrLn $ weightsToString oW
  let bb = best4pcBuilds oW sz onA offA
  prntBestBuilds 3 bb
  return (n+1,calcStatWeights bb oW)

char :: Character
char = furina --this is global current char

makeDataset :: Character -> (Int,Color,[(Int, Build)]) -> (String, Color, [(Int, Double, [String])])
makeDataset c (indexLabel,clr,lst) = (show indexLabel,clr,map makePoint lst)
  --where makePoint (i,b) = (i, damage b, [])
  where makePoint (i,b) = (i, damage b, buildHeader (i,b) : map prettyPrint b)

makeProgression :: Character -> Int -> Int -> IO [(Int, Build)]
makeProgression c artN _ = do
    setArts <- generateArtifacts "GT" artN
    offArts <- generateArtifacts "MS" artN
    let prg = progression c (bestBuild 7) setArts offArts
    putStrLn$ "AllBuilds: "++show (length prg)
    return prg

main :: IO ()
main = do
  let threads = 3
  let mlt = 10
  start <- getCurrentTime
  let mapCB = mapConcurrentlyBounded threads

  results <- mapCB (makeProgression char (1000*mlt)) [0..10*mlt]
  print$map fst (head results)
  let probes = map (*mlt) [1,5,9]
  let colorScheme = [(255,99,132),(99,255,132),(132,99,255)]
  let aggStats = statistics (dmgClc char []) probes results
  print$map length aggStats
  --mapM_ prntBuildWithIndex (head aggStats)
  let plotData = zip3 probes colorScheme (map tail aggStats)
  writePlotData "data/plot.json" (map (makeDataset char) plotData)

  end <- getCurrentTime
  let duration = realToFrac (diffUTCTime end start)
  putStrLn "Ended in: "
  putStrLn $ show duration ++ " seconds"


module GeneratorUtils where


import System.Random (mkStdGen, setStdGen, getStdGen)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import ArtifactType
import Generator
import Character
import CharacterBuild


type BuildMaker = [Artifact] -> [Artifact] -> [[Artifact]]

-- Measure the time an IO operation takes
whileMeasuringTime :: IO a -> IO (Double,a)
whileMeasuringTime action = do
  start <- getCurrentTime  -- Record start time
  result <- action         -- Perform the action
  end <- getCurrentTime    -- Record end time
  let duration = realToFrac (diffUTCTime end start)  -- Calculate duration in seconds
  return (duration, result)

-- Function to wrap another function in a deterministic random environment
withDeterministicRandom :: Int -> IO a -> IO a
withDeterministicRandom seed action = do
  oldGen <- getStdGen
  let customGen = mkStdGen seed
  setStdGen customGen
  result <- action
  setStdGen oldGen
  return result

--Generate artifacts based on a seed, number set at 10000 to ensure consistency
artifactsFromSeed :: Int -> IO ([Artifact], [Artifact])
artifactsFromSeed seed =
    withDeterministicRandom seed $ do
        setA <- generateArtifacts "GT" artN
        offA <- generateArtifacts "MS" artN
        return (setA, offA)
    where artN = 10000

buildMakerDiff::([Artifact]->Double)->Int->BuildMaker->BuildMaker->Int->IO Double
buildMakerDiff dmgCalc artN buildMaker1 buildMaker2 seed =
    withDeterministicRandom seed $ do
        setA <- generateArtifacts "GT" artN
        offA <- generateArtifacts "MS" artN
        let pfS = paretoFilterReal furina setA
        let pfO = paretoFilterReal furina offA
        let res bm = maximum.map dmgCalc $ bm setA offA
        let res1 = res buildMaker1
        let res2 = res buildMaker2
        let r = res2 - res1 in if r == r then return r else return r

damageFromSeed :: ([Artifact]->Double)->Int->BuildMaker->Int->IO Double
damageFromSeed dmgCalc artN buildMaker seed = 
    withDeterministicRandom seed $ do
        setA <- generateArtifacts "GT" artN
        offA <- generateArtifacts "MS" artN
        let builds = buildMaker setA offA
        return (maximum.map dmgCalc $ builds)

buildsFromSeed :: ([Artifact]->Double)->[Artifact]->[Artifact]->Int->BuildMaker->Int->IO Double
buildsFromSeed dmgCalc iSetA iOffA artN buildMaker seed =
    withDeterministicRandom seed $ do
        setA <- generateArtifacts "GT" artN
        offA <- generateArtifacts "MS" artN
        let setArts = iSetA ++ setA
        let offArts = iOffA ++ offA
        let builds = buildMaker setArts offArts
        return (maximum.map dmgCalc $ builds)


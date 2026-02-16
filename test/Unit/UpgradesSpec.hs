module Unit.UpgradesSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Artifact
import Character
import Core.Utils (ArtifactInfo(..), toArtifactInfo, aiStatline)
import Core.Upgrades (trackLastSeen, aggregateStats, UpgradeStats(..))

-- Test helpers
mkTestArtifact :: Piece -> Stat -> String -> Artifact
mkTestArtifact p m s = Artifact
  { piece = p
  , set = s
  , stats = [(m, 100.0), (CR, 10.0), (CD, 20.0)]
  , upNumber = 4
  }

spec :: Spec
spec = do
  describe "ArtifactInfo Map operations" $ do
    it "inserts and retrieves ArtifactInfo correctly" $ do
      let art1 = mkTestArtifact Flower HP "TestSet"
          -- Create dummy character for conversion (uses furina as placeholder)
          artInfo1 = toArtifactInfo undefined art1  -- Character arg unused for structure
          
          testMap = Map.singleton artInfo1 100
          
      Map.lookup artInfo1 testMap `shouldBe` Just 100
    
    it "handles multiple artifacts with same stats but different sets" $ do
      let art1 = mkTestArtifact Flower HP "SetA"
          art2 = mkTestArtifact Flower HP "SetB"
          artInfo1 = toArtifactInfo undefined art1
          artInfo2 = toArtifactInfo undefined art2
          
          testMap = Map.fromList [(artInfo1, 100), (artInfo2, 200)]
      
      -- Should be different keys due to set difference
      Map.lookup artInfo1 testMap `shouldBe` Just 100
      Map.lookup artInfo2 testMap `shouldBe` Just 200
      Map.size testMap `shouldBe` 2
    
    it "overwrites when inserting same artifact twice" $ do
      let art1 = mkTestArtifact Flower HP "TestSet"
          artInfo1 = toArtifactInfo undefined art1
          
          testMap = Map.insert artInfo1 200 $ Map.singleton artInfo1 100
      
      Map.lookup artInfo1 testMap `shouldBe` Just 200
      Map.size testMap `shouldBe` 1
  
  describe "trackLastSeen" $ do
    it "tracks artifact appearance in single build" $ do
      let art1 = mkTestArtifact Flower HP "TestSet"
          art2 = mkTestArtifact Plume ATK "TestSet"
          artInfo1 = toArtifactInfo undefined art1
          artInfo2 = toArtifactInfo undefined art2
          
          progression = [(0, [artInfo1, artInfo2])]
          result = trackLastSeen progression
      
      Map.lookup artInfo1 result `shouldBe` Just 0
      Map.lookup artInfo2 result `shouldBe` Just 0
    
    it "updates last-seen index when artifact appears multiple times" $ do
      let art1 = mkTestArtifact Flower HP "TestSet"
          artInfo1 = toArtifactInfo undefined art1
          
          progression = [(0, [artInfo1]), (100, [artInfo1]), (500, [artInfo1])]
          result = trackLastSeen progression
      
      -- Should be last appearance
      Map.lookup artInfo1 result `shouldBe` Just 500
    
    it "handles artifacts appearing and disappearing" $ do
      let art1 = mkTestArtifact Flower HP "TestSet"
          art2 = mkTestArtifact Plume ATK "TestSet"
          artInfo1 = toArtifactInfo undefined art1
          artInfo2 = toArtifactInfo undefined art2
          
          -- art1 appears early, art2 appears later
          progression = [(0, [artInfo1]), (100, [artInfo1, artInfo2]), (500, [artInfo2])]
          result = trackLastSeen progression
      
      Map.lookup artInfo1 result `shouldBe` Just 100  -- Last seen at 100
      Map.lookup artInfo2 result `shouldBe` Just 500  -- Last seen at 500
  
  describe "aggregateStats" $ do
    it "handles artifacts that never appear" $ do
      let art1 = mkTestArtifact Flower HP "TestSet"
          artInfo1 = toArtifactInfo undefined art1
          
          -- Empty tracking maps
          stats = aggregateStats [artInfo1] [Map.empty, Map.empty]
          result = head stats
      
      usReplacementIndices result `shouldBe` []
      usMedianReplacement result `shouldBe` 0
      usProbability result `shouldBe` 0.0
    
    it "computes correct probability across runs" $ do
      let art1 = mkTestArtifact Flower HP "TestSet"
          artInfo1 = toArtifactInfo undefined art1
          
          -- Appears in 2 out of 4 runs
          maps = [ Map.singleton artInfo1 100
                 , Map.empty
                 , Map.singleton artInfo1 200
                 , Map.empty
                 ]
          stats = aggregateStats [artInfo1] maps
          result = head stats
      
      length (usReplacementIndices result) `shouldBe` 2
      usProbability result `shouldBe` 0.5
    
    it "computes correct median" $ do
      let art1 = mkTestArtifact Flower HP "TestSet"
          artInfo1 = toArtifactInfo undefined art1
          
          -- Appears at different indices
          maps = [ Map.singleton artInfo1 100
                 , Map.singleton artInfo1 200
                 , Map.singleton artInfo1 300
                 ]
          stats = aggregateStats [artInfo1] maps
          result = head stats
      
      usMedianReplacement result `shouldBe` 200
      usProbability result `shouldBe` 1.0

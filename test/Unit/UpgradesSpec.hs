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
  
  describe "full tracking simulation" $ do
    it "simulates progression with real and generated artifacts" $ do
      -- Setup: 3 real artifacts, 2 generated (with different stats)
      let realArt1 = mkTestArtifact Flower HP "TestSet"  -- CR:10, CD:20
          realArt2 = mkTestArtifact Plume ATK "TestSet"
          realArt3 = mkTestArtifact Sands ER "TestSet"
          -- Generated artifacts have better substats
          genArt1 = Artifact { piece = Flower, set = "TestSet", stats = [(HP, 100.0), (CR, 15.0), (CD, 25.0)], upNumber = 4 }
          genArt2 = mkTestArtifact Goblet DMG "TestSet"
          
          realInfo1 = toArtifactInfo undefined realArt1
          realInfo2 = toArtifactInfo undefined realArt2
          realInfo3 = toArtifactInfo undefined realArt3
          genInfo1 = toArtifactInfo undefined genArt1
          genInfo2 = toArtifactInfo undefined genArt2
          
          allRealInfo = [realInfo1, realInfo2, realInfo3]
          
          -- Simulate progression:
          -- Index 0: Initial build with real artifacts
          -- Index 150: genArt1 replaces realArt1
          -- Index 300: genArt2 joins
          -- Index 500: Final state
          progression = 
            [ (0, [realInfo1, realInfo2, realInfo3])
            , (150, [genInfo1, realInfo2, realInfo3])  -- realInfo1 replaced
            , (300, [genInfo1, realInfo2, realInfo3, genInfo2])
            , (500, [genInfo1, realInfo2, genInfo2])  -- realInfo3 gone
            ]
          
          tracked = trackLastSeen progression
          stats = aggregateStats allRealInfo [tracked]
      
      -- Verify tracking
      Map.lookup realInfo1 tracked `shouldBe` Just 0    -- Replaced immediately
      Map.lookup realInfo2 tracked `shouldBe` Just 500  -- Kept till end
      Map.lookup realInfo3 tracked `shouldBe` Just 300  -- Replaced at 300
      Map.lookup genInfo1 tracked `shouldBe` Just 500   -- Generated, in final
      Map.lookup genInfo2 tracked `shouldBe` Just 500   -- Generated, in final
      
      -- Verify stats for real artifacts only
      length stats `shouldBe` 3
      let [stat1, stat2, stat3] = stats
      
      -- realInfo1: appeared once, replaced at 0
      usReplacementIndices stat1 `shouldBe` [0]
      usMedianReplacement stat1 `shouldBe` 0
      usProbability stat1 `shouldBe` 1.0
      
      -- realInfo2: never replaced
      usReplacementIndices stat2 `shouldBe` [500]
      usMedianReplacement stat2 `shouldBe` 500
      usProbability stat2 `shouldBe` 1.0
      
      -- realInfo3: replaced at 300
      usReplacementIndices stat3 `shouldBe` [300]
      usMedianReplacement stat3 `shouldBe` 300
      usProbability stat3 `shouldBe` 1.0
    
    it "tracks hidden gem that appears only in some runs" $ do
      let realArt1 = mkTestArtifact Flower HP "TestSet"
          realArt2 = mkTestArtifact Plume ATK "TestSet"
          realArt3 = mkTestArtifact Sands ER "TestSet"  -- Hidden gem
          
          realInfo1 = toArtifactInfo undefined realArt1
          realInfo2 = toArtifactInfo undefined realArt2
          realInfo3 = toArtifactInfo undefined realArt3
          
          allRealInfo = [realInfo1, realInfo2, realInfo3]
          
          -- Run 1: realInfo3 never appears
          prog1 = [(0, [realInfo1, realInfo2]), (500, [realInfo1, realInfo2])]
          
          -- Run 2: realInfo3 becomes useful at 200
          prog2 = [(0, [realInfo1, realInfo2]), (200, [realInfo1, realInfo2, realInfo3]), (500, [realInfo1, realInfo2, realInfo3])]
          
          -- Run 3: realInfo3 appears early, stays till end
          prog3 = [(0, [realInfo1, realInfo2, realInfo3]), (500, [realInfo1, realInfo2, realInfo3])]
          
          tracked1 = trackLastSeen prog1
          tracked2 = trackLastSeen prog2
          tracked3 = trackLastSeen prog3
          
          stats = aggregateStats allRealInfo [tracked1, tracked2, tracked3]
          stat3 = stats !! 2  -- Stats for realInfo3
      
      -- realInfo3 appears in 2 out of 3 runs
      length (usReplacementIndices stat3) `shouldBe` 2
      usProbability stat3 `shouldBe` (2.0 / 3.0)
      usMedianReplacement stat3 `shouldBe` 500  -- Median of [500, 500]
      
      -- Rating should reflect it's a hidden gem
      usRating stat3 `shouldBe` 500 * (2.0 / 3.0)
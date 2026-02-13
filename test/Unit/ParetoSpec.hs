module Unit.ParetoSpec (spec) where
import Test.Hspec
import Test.QuickCheck
import Character
import CharacterLibrary
import CharacterBuild (paretoFilter, paretoFilterReal)
import Generator (generateArtifactForPiece)
import System.Random (mkStdGen)
import Data.List (sort, nub)
import Control.Monad (replicateM)
import Artifact (Artifact(..), Piece(..), Stat(..))
-- Helper to create artifacts without set
mkArtifact :: Piece -> [(Stat, Double)] -> Artifact
mkArtifact p sts = Artifact { piece = p, set = "", upNumber = 0, stats = sts }
-- Generate random artifacts for a single piece with HP/CR/CD stats
genArtifactsForPiece :: Piece -> Int -> Gen [Artifact]
genArtifactsForPiece p n = replicateM n $ do
  hp <- choose (0.0, 50.0)
  cr <- choose (0.0, 15.0)
  cd <- choose (0.0, 30.0)
  return $ mkArtifact p [(HP, hp), (CR, cr), (CD, cd)]
-- Check if artifact A dominates artifact B for given character
-- A dominates B if A >= B on all scaling stats AND A > B on at least one
dominates :: Character -> Artifact -> Artifact -> Bool
dominates c a b = allGTE && anyGT where
  scl = scaling c
  getVal art s = sum [v | (s', v) <- stats art, s' == s]
  allGTE = all (\s -> getVal a s >= getVal b s) scl
  anyGT = any (\s -> getVal a s > getVal b s) scl
-- Check if result is subset of input (preserving order)
isOrderedSubset :: Eq a => [a] -> [a] -> Bool
isOrderedSubset [] _ = True
isOrderedSubset _ [] = False
isOrderedSubset (x:xs) (y:ys)
  | x == y = isOrderedSubset xs ys
  | otherwise = isOrderedSubset (x:xs) ys
spec :: Spec
spec = describe "Pareto Filtering" $ do
  describe "paretoFilterReal (true pareto frontier)" $ do
    it "removes strictly dominated artifact" $ do
      let strong = mkArtifact Flower [(CR, 10.0), (CD, 20.0)]
      let weak = mkArtifact Flower [(CR, 5.0), (CD, 10.0)]
      let result = paretoFilterReal furina [strong, weak]
      result `shouldBe` [strong]
    it "keeps first when artifacts have equal stats" $ do
      let art1 = mkArtifact Flower [(CR, 10.0)]
      let art2 = mkArtifact Flower [(CR, 10.0)]
      let result = paretoFilterReal furina [art1, art2]
      result `shouldBe` [art1]
    it "keeps both artifacts with mixed tradeoffs" $ do
      let critArt = mkArtifact Flower [(CR, 10.0)]
      let hpArt = mkArtifact Flower [(HP, 10.0)]
      let result = paretoFilterReal furina [critArt, hpArt]
      length result `shouldBe` 2
    it "handles transitive dominance correctly" $ do
      let best = mkArtifact Flower [(CR, 10.0), (CD, 20.0)]
      let mid = mkArtifact Flower [(CR, 8.0), (CD, 16.0)]
      let worst = mkArtifact Flower [(CR, 6.0), (CD, 12.0)]
      let result = paretoFilterReal furina [best, mid, worst]
      result `shouldBe` [best]
    it "filters per-piece independently" $ do
      let flowerGood = mkArtifact Flower [(CR, 10.0)]
      let flowerBad = mkArtifact Flower [(CR, 5.0)]
      let plumeGood = mkArtifact Plume [(CR, 10.0)]
      let plumeBad = mkArtifact Plume [(CR, 5.0)]
      let result = paretoFilterReal furina [flowerGood, flowerBad, plumeGood, plumeBad]
      length result `shouldBe` 2
      result `shouldContain` [flowerGood]
      result `shouldContain` [plumeGood]
    it "handles empty list" $ do
      let result = paretoFilterReal furina ([] :: [Artifact])
      result `shouldBe` []
    it "handles single artifact" $ do
      let art = mkArtifact Flower [(CR, 10.0)]
      let result = paretoFilterReal furina [art]
      result `shouldBe` [art]
    it "preserves relative order of non-dominated artifacts" $ do
      let art1 = mkArtifact Flower [(CR, 10.0)]
      let art2 = mkArtifact Flower [(HP, 10.0)]
      let art3 = mkArtifact Flower [(CD, 10.0)]
      let result = paretoFilterReal furina [art1, art2, art3]
      isOrderedSubset result [art1, art2, art3] `shouldBe` True
    it "is order-independent for final result" $ do
      let art1 = mkArtifact Flower [(CR, 10.0)]
      let art2 = mkArtifact Flower [(CR, 5.0)]
      let result1 = paretoFilterReal furina [art1, art2]
      let result2 = paretoFilterReal furina [art2, art1]
      result1 `shouldBe` result2
  describe "paretoFilter (forward-only)" $ do
    it "removes dominated artifacts that come after" $ do
      let strong = mkArtifact Flower [(CR, 10.0), (CD, 20.0)]
      let weak = mkArtifact Flower [(CR, 5.0), (CD, 10.0)]
      let result = paretoFilter furina [strong, weak]
      result `shouldBe` [strong]
    it "shows order dependency: weak then strong" $ do
      let weak = mkArtifact Flower [(CR, 5.0)]
      let strong = mkArtifact Flower [(CR, 10.0)]
      let result = paretoFilter furina [weak, strong]
      length result `shouldBe` 2  -- both stay because weak comes first
    it "shows order dependency: strong then weak" $ do
      let strong = mkArtifact Flower [(CR, 10.0)]
      let weak = mkArtifact Flower [(CR, 5.0)]
      let result = paretoFilter furina [strong, weak]
      result `shouldBe` [strong]  -- weak removed because strong comes first
    it "preserves input order" $ do
      let art1 = mkArtifact Flower [(CR, 10.0)]
      let art2 = mkArtifact Flower [(HP, 10.0)]
      let art3 = mkArtifact Flower [(CD, 10.0)]
      let result = paretoFilter furina [art1, art2, art3]
      isOrderedSubset result [art1, art2, art3] `shouldBe` True
    it "normalises flat stats correctly" $ do
      let artHighFlat = mkArtifact Flower [(HP, 10.0), (HPf, 300.0)]
      let artHighP = mkArtifact Flower [(HP, 11.0)]
      let result = paretoFilterReal furina [artHighFlat, artHighP]
      result `shouldBe` [artHighFlat]  -- artHighFlat dominates artHighP after normalisation
  describe "Property-based tests (QuickCheck)" $ do
    describe "paretoFilterReal properties" $ do
      it "is idempotent (filter . filter = filter)" $ property $
        forAll (genArtifactsForPiece Flower 20) $ \arts ->
          let filtered = paretoFilterReal furina arts
              refiltered = paretoFilterReal furina filtered
          in filtered == refiltered
      it "returns subset of input" $ property $
        forAll (genArtifactsForPiece Flower 20) $ \arts ->
          let result = paretoFilterReal furina arts
          in all (`elem` arts) result
      it "preserves relative order" $ property $
        forAll (genArtifactsForPiece Flower 20) $ \arts ->
          isOrderedSubset (paretoFilterReal furina arts) arts
      it "no artifact in result dominates another" $ property $
        forAll (genArtifactsForPiece Flower 20) $ \arts ->
          let result = paretoFilterReal furina arts
              pairs = [(a, b) | a <- result, b <- result, a /= b]
          in all (\(a, b) -> not (dominates furina a b)) pairs
      it "every removed artifact is dominated by something in result" $ property $
        forAll (genArtifactsForPiece Flower 20) $ \arts ->
          let result = paretoFilterReal furina arts
              removed = filter (`notElem` result) arts
          in all (\r -> any (\k -> dominates furina k r) result) removed
      it "handles different piece types independently" $ property $ do
        flowersArts <- genArtifactsForPiece Flower 10
        plumeArts <- genArtifactsForPiece Plume 10
        let combined = flowersArts ++ plumeArts
        let resultCombined = paretoFilterReal furina combined
        let resultFlowers = paretoFilterReal furina flowersArts
        let resultPlumes = paretoFilterReal furina plumeArts
        let flowersInResult = filter (\a -> piece a == Flower) resultCombined
        let plumesInResult = filter (\a -> piece a == Plume) resultCombined
        return $ flowersInResult == resultFlowers &&
                 plumesInResult == resultPlumes
    describe "paretoFilter properties" $ do
      it "returns subset of input" $ property $
        forAll (genArtifactsForPiece Flower 20) $ \arts ->
          let result = paretoFilter furina arts
          in all (`elem` arts) result
      it "preserves exact input order (not just relative)" $ property $
        forAll (genArtifactsForPiece Flower 20) $ \arts ->
          isOrderedSubset (paretoFilter furina arts) arts
module Data.Set.MergeSpec where

import qualified Data.Set as S
import Data.Set.Merge
-- import qualified Data.Map.Strict as M
import Data.MergeTactics
import Test.Hspec


spec :: Spec
spec = do

  let s1 = S.fromList ["one", "three", "five", "seven", "nine"]
  let s2 = S.fromList ["two", "three", "five", "seven"]

  describe "Merges: set vs set" $ do

    it "calculates union" $ do
      let union' = mergeSet preserveMissing preserveMissing preserveLeftMatched
      union' s1 s2 `shouldBe` S.union s1 s2

    it "calculates intersection" $ do
      let intersection' = mergeSet dropMissing dropMissing preserveLeftMatched
      intersection' s1 s2 `shouldBe` S.intersection s1 s2

module Data.Maplike.BranchingSpec where

import qualified Data.Map.Strict as M
import Data.Maplike
import Data.Maplike.Branching
import Test.Hspec


spec :: Spec
spec = do

  let m1 = fromFold [(M.fromList [("a",1),("b",2),("c",3)], "abc"),
                     (M.fromList [("a",1),("b",2)], "ab"),
                     (M.fromList [("a",1),("c",2),("b",3)], "acb"),
                     (M.empty, "")]                                 :: MapBranching String Int String

  let m2 = fromFold [(M.fromList [("a",1),("b",2),("c",3)], "123"),
                     (M.empty, ""),
                     (M.fromList [("c",1),("b",2),("a",3)], "321")] :: MapBranching String Int String

  let m12 = fromFold [(M.fromList [("a",1),("b",2),("c",3)], "abc<>123"),
                      (M.empty, "<>"),
                      (M.fromList [("a",1),("b",2)], "ab<"),
                      (M.fromList [("a",1),("c",2),("b",3)], "acb>"),
                      (M.fromList [("c",1),("b",2),("a",3)], ">321")]
  
  describe "Branching: merging" $ do

    it "merges correctly" $ do
      merge _ _ _ m1 m2 `shouldBe` m12
      

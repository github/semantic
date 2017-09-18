{-# LANGUAGE DataKinds #-}
module InterpreterSpec where

import Category
import Data.Functor.Both
import Data.Functor.Foldable hiding (Nil)
import Data.Functor.Listable
import Data.Record
import Diff
import Interpreter
import Syntax
import Term
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "interpret" $ do
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = Term $ (StringLiteral :. Nil) `In` Leaf "t\776"
          termB = Term $ (StringLiteral :. Nil) `In` Leaf "\7831" in
          diffTerms termA termB `shouldBe` replacing termA termB

    prop "produces correct diffs" $
      \ a b -> let diff = diffTerms a b :: Diff Syntax (Record '[Category]) (Record '[Category]) in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just a, Just b)

    prop "constructs zero-cost diffs of equal terms" $
      \ a -> let diff = diffTerms a a :: Diff Syntax (Record '[Category]) (Record '[Category]) in
                 diffCost diff `shouldBe` 0

    it "produces unbiased insertions within branches" $
      let term s = Term ((StringLiteral :. Nil) `In` Indexed [ Term ((StringLiteral :. Nil) `In` Leaf s) ]) :: Term Syntax (Record '[Category])
          root = termIn (Program :. Nil) . Indexed in
      diffTerms (root [ term "b" ]) (root [ term "a", term "b" ]) `shouldBe` merge ((Program :. Nil, Program :. Nil)) (Indexed [ inserting (term "a"), cata (\ (In a r) -> merge (a, a) r) (term "b") ])

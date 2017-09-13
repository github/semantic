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
          diffTerms (both termA termB) `shouldBe` replacing termA termB

    prop "produces correct diffs" $
      \ a b -> let diff = diffTerms (unListableF <$> both a b :: Both (Term Syntax (Record '[Category]))) in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just (unListableF a), Just (unListableF b))

    prop "constructs zero-cost diffs of equal terms" $
      \ a -> let term = (unListableF a :: Term Syntax (Record '[Category]))
                 diff = diffTerms (pure term) in
                 diffCost diff `shouldBe` 0

    it "produces unbiased insertions within branches" $
      let term s = Term ((StringLiteral :. Nil) `In` Indexed [ Term ((StringLiteral :. Nil) `In` Leaf s) ]) :: Term Syntax (Record '[Category])
          root = termIn (Program :. Nil) . Indexed in
      diffTerms (both (root [ term "b" ]) (root [ term "a", term "b" ])) `shouldBe` merge ((Program :. Nil, Program :. Nil)) (Indexed [ inserting (term "a"), cata (\ (In a r) -> merge (a, a) r) (term "b") ])

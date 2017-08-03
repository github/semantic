{-# LANGUAGE DataKinds #-}
module InterpreterSpec where

import Category
import Control.Monad.Free (wrap)
import Data.Functor.Both
import Data.Functor.Foldable hiding (Nil)
import Data.Functor.Listable
import Data.Record
import Diff
import Interpreter
import Patch
import Syntax
import Term
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "interpret" $ do
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = cofree $ (StringLiteral :. Nil) :< Leaf "t\776"
          termB = cofree $ (StringLiteral :. Nil) :< Leaf "\7831" in
          diffTerms (both termA termB) `shouldBe` replacing termA termB

    prop "produces correct diffs" $
      \ a b -> let diff = diffTerms (unListableF <$> both a b :: Both (SyntaxTerm '[Category])) in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just (unListableF a), Just (unListableF b))

    prop "constructs zero-cost diffs of equal terms" $
      \ a -> let term = (unListableF a :: SyntaxTerm '[Category])
                 diff = diffTerms (pure term) in
                 diffCost diff `shouldBe` 0

    it "produces unbiased insertions within branches" $
      let term s = cofree ((StringLiteral :. Nil) :< Indexed [ cofree ((StringLiteral :. Nil) :< Leaf s) ]) :: SyntaxTerm '[Category]
          root = cofree . ((Program :. Nil) :<) . Indexed in
      diffTerms (both (root [ term "b" ]) (root [ term "a", term "b" ])) `shouldBe` wrap (pure (Program :. Nil) :< Indexed [ inserting (term "a"), cata wrap (fmap pure (term "b")) ])

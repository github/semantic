{-# LANGUAGE DataKinds #-}
module InterpreterSpec where

import Data.Diff
import Data.Functor.Both
import Data.Functor.Foldable hiding (Nil)
import Data.Functor.Listable
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Term
import Data.Union
import Interpreter
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "diffTerms" $ do
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = termIn Nil (inj (Syntax.Identifier "t\776"))
          termB = termIn Nil (inj (Syntax.Identifier "\7831")) in
          diffTerms termA termB `shouldBe` replacing termA (termB :: Term ListableSyntax (Record '[]))

    prop "produces correct diffs" $
      \ a b -> let diff = diffTerms a b :: Diff ListableSyntax (Record '[]) (Record '[]) in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just a, Just b)

    prop "produces identity diffs for equal terms " $
      \ a -> let diff = diffTerms a a :: Diff ListableSyntax (Record '[]) (Record '[]) in
                 length (diffPatches diff) `shouldBe` 0

    it "produces unbiased insertions within branches" $
      let term s = termIn Nil (inj [ termIn Nil (inj (Syntax.Identifier s)) ]) :: Term ListableSyntax (Record '[])
          wrap = termIn Nil . inj in
      diffTerms (wrap [ term "b" ]) (wrap [ term "a", term "b" ]) `shouldBe` merge (Nil, Nil) (inj [ inserting (term "a"), merging (term "b") ])

    prop "compares nodes against context" $
      \ a b -> diffTerms a (termIn Nil (inj (Syntax.Context (pure b) a))) `shouldBe` insertF (In Nil (inj (Syntax.Context (pure (inserting b)) (merging (a :: Term ListableSyntax (Record '[]))))))

    prop "diffs forward permutations as changes" $
      \ a -> let wrap = termIn Nil . inj
                 b = wrap [a]
                 c = wrap [a, b] in
        diffTerms (wrap [a, b, c]) (wrap [c, a, b :: Term ListableSyntax (Record '[])]) `shouldBe` merge (Nil, Nil) (inj [ inserting c, merging a, merging b, deleting c ])

    prop "diffs backward permutations as changes" $
      \ a -> let wrap = termIn Nil . inj
                 b = wrap [a]
                 c = wrap [a, b] in
        diffTerms (wrap [a, b, c]) (wrap [b, c, a :: Term ListableSyntax (Record '[])]) `shouldBe` merge (Nil, Nil) (inj [ deleting a, merging b, merging c, inserting a ])

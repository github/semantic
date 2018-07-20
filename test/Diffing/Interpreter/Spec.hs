{-# LANGUAGE DataKinds #-}
module Diffing.Interpreter.Spec where

import Data.Diff
import Data.Functor.Listable
import Data.Maybe
import Data.Record
import Data.Sum
import Data.Term
import Data.These
import Diffing.Interpreter
import qualified Data.Syntax as Syntax
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck.Core
import SpecHelpers

spec :: Spec
spec = parallel $ do
  describe "diffTerms" $ do
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = termIn Nil (inject (Syntax.Identifier "t\776"))
          termB = termIn Nil (inject (Syntax.Identifier "\7831")) in
          diffTerms termA termB `shouldBe` replacing termA (termB :: Term ListableSyntax (Record '[]))

    prop "produces correct diffs" $
      \ a b -> let diff = diffTerms a b :: Diff ListableSyntax (Record '[]) (Record '[]) in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just a, Just b)

    prop "produces identity diffs for equal terms " $
      \ a -> let diff = diffTerms a a :: Diff ListableSyntax (Record '[]) (Record '[]) in
                 length (diffPatches diff) `shouldBe` 0

    it "produces unbiased insertions within branches" $
      let term s = termIn Nil (inject [ termIn Nil (inject (Syntax.Identifier s)) ]) :: Term ListableSyntax (Record '[])
          wrap = termIn Nil . inject in
      diffTerms (wrap [ term "b" ]) (wrap [ term "a", term "b" ]) `shouldBe` merge (Nil, Nil) (inject [ inserting (term "a"), merging (term "b") ])

    let noContext :: Term ListableSyntax a -> Bool
        noContext = isNothing . project @Syntax.Context . termOut

    prop "compares nodes against context" . forAll (filterT (noContext . fst) tiers) $
      \ (a, b) -> diffTerms a (termIn Nil (inject (Syntax.Context (pure b) a))) `shouldBe` insertF (In Nil (inject (Syntax.Context (pure (inserting b)) (merging (a :: Term ListableSyntax (Record '[]))))))

    prop "diffs forward permutations as changes" $
      \ a -> let wrap = termIn Nil . inject
                 b = wrap [a]
                 c = wrap [a, b] in
        diffTerms (wrap [a, b, c]) (wrap [c, a, b :: Term ListableSyntax (Record '[])]) `shouldBe` merge (Nil, Nil) (inject [ inserting c, merging a, merging b, deleting c ])

    prop "diffs backward permutations as changes" $
      \ a -> let wrap = termIn Nil . inject
                 b = wrap [a]
                 c = wrap [a, b] in
        diffTerms (wrap [a, b, c]) (wrap [b, c, a :: Term ListableSyntax (Record '[])]) `shouldBe` merge (Nil, Nil) (inject [ deleting a, merging b, merging c, inserting a ])

  describe "diffTermPair" $ do
    prop "produces an Insert when the first term is missing" $ do
      \ after -> let diff = diffTermPair (That after) :: Diff ListableSyntax (Record '[]) (Record '[]) in
        diff `shouldBe` inserting after

    prop "produces a Delete when the second term is missing" $ do
      \ before -> let diff = diffTermPair (This before) :: Diff ListableSyntax (Record '[]) (Record '[]) in
        diff `shouldBe` deleting before


-- | Recover the before state of a diff.
beforeTerm :: (Foldable syntax, Mergeable syntax) => Diff syntax ann1 ann2 -> Maybe (Term syntax ann1)
beforeTerm = cata $ \ diff -> case diff of
  Patch patch -> (before patch >>= \ (In  a     l) -> termIn a <$> sequenceAlt l) <|> (after patch >>= asum)
  Merge                              (In (a, _) l) -> termIn a <$> sequenceAlt l

-- | Recover the after state of a diff.
afterTerm :: (Foldable syntax, Mergeable syntax) => Diff syntax ann1 ann2 -> Maybe (Term syntax ann2)
afterTerm = cata $ \ diff -> case diff of
  Patch patch -> (after patch >>= \ (In     b  r) -> termIn b <$> sequenceAlt r) <|> (before patch >>= asum)
  Merge                             (In (_, b) r) -> termIn b <$> sequenceAlt r

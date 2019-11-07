{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
module Diffing.Interpreter.Spec (spec, afterTerm, beforeTerm) where

import Control.Applicative ((<|>))
import Data.Diff
import Data.Foldable (asum)
import Data.Functor.Foldable (cata)
import Data.Functor.Listable
import Data.Maybe
import Data.Mergeable
import Data.Sum
import Data.Term
import Diffing.Interpreter
import qualified Data.Syntax as Syntax
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations
import Test.Hspec.LeanCheck
import Test.LeanCheck.Core
import SpecHelpers (Edit(..), edit)

spec :: Spec
spec = do
  describe "diffTerms" $ do
    it "returns a replacement when comparing two unicode equivalent terms" $
      let termA = termIn emptyAnnotation (inject (Syntax.Identifier "t\776"))
          termB = termIn emptyAnnotation (inject (Syntax.Identifier "\7831")) in
          diffTerms termA termB `shouldBe` comparing termA (termB :: Term ListableSyntax ())

    prop "produces correct diffs" $
      \ a b -> let diff = diffTerms a b :: Diff ListableSyntax () () in
                   (beforeTerm diff, afterTerm diff) `shouldBe` (Just a, Just b)

    prop "produces identity diffs for equal terms " $
      \ a -> let diff = diffTerms a a :: Diff ListableSyntax () () in
                 length (diffPatches diff) `shouldBe` 0

    it "produces unbiased insertions within branches" $
      let term s = termIn emptyAnnotation (inject [ termIn emptyAnnotation (inject (Syntax.Identifier s)) ]) :: Term ListableSyntax ()
          wrap = termIn emptyAnnotation . inject in
      diffTerms (wrap [ term "b" ]) (wrap [ term "a", term "b" ]) `shouldBe` merge (emptyAnnotation, emptyAnnotation) (inject [ inserting (term "a"), merging (term "b") ])

    let noContext :: Term ListableSyntax a -> Bool
        noContext = isNothing . project @Syntax.Context . termOut

    prop "compares nodes against context" . forAll (filterT (noContext . fst) tiers) $
      \ (a, b) -> diffTerms a (termIn emptyAnnotation (inject (Syntax.Context (pure b) a))) `shouldBe` insertF (In emptyAnnotation (inject (Syntax.Context (pure (inserting b)) (merging (a :: Term ListableSyntax ())))))

    prop "diffs forward permutations as changes" $
      \ a -> let wrap = termIn emptyAnnotation . inject
                 b = wrap [a]
                 c = wrap [a, b] in
        diffTerms (wrap [a, b, c]) (wrap [c, a, b :: Term ListableSyntax ()]) `shouldBe` merge (emptyAnnotation, emptyAnnotation) (inject [ inserting c, merging a, merging b, deleting c ])

    prop "diffs backward permutations as changes" $
      \ a -> let wrap = termIn emptyAnnotation . inject
                 b = wrap [a]
                 c = wrap [a, b] in
        diffTerms (wrap [a, b, c]) (wrap [b, c, a :: Term ListableSyntax ()]) `shouldBe` merge (emptyAnnotation, emptyAnnotation) (inject [ deleting a, merging b, merging c, inserting a ])

  describe "diffTermPair" $ do
    prop "produces an Insert when the first term is missing" $ do
      \ after -> let diff = diffTermPair (Insert after) :: Diff ListableSyntax () () in
        diff `shouldBe` inserting after

    prop "produces a Delete when the second term is missing" $ do
      \ before -> let diff = diffTermPair (Delete before) :: Diff ListableSyntax () () in
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

-- | Return the item from the after side of the patch.
after :: Edit l r -> Maybe r
after = edit (const Nothing) Just (\ _ b -> Just b)

-- | Return the item from the before side of the patch.
before :: Edit l r -> Maybe l
before = edit Just (const Nothing) (\ a _ -> Just a)

emptyAnnotation :: ()
emptyAnnotation = ()

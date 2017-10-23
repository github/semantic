{-# LANGUAGE DataKinds #-}
module DiffSpec where

import Data.Diff
import Data.Functor.Both
import Data.Functor.Foldable (cata)
import Data.Functor.Listable (ListableSyntax)
import Data.Record
import Data.Term
import Data.Union
import Interpreter
import RWS
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  prop "equality is reflexive" $
    \ diff -> diff `shouldBe` (diff :: Diff ListableSyntax (Record '[]) (Record '[]))

  prop "equal terms produce identity diffs" $
    \ term -> length (diffPatches (diffTerms term (term :: Term ListableSyntax (Record '[])))) `shouldBe` 0

  describe "beforeTerm" $ do
    prop "recovers the before term" $
      \ a b -> let diff = diffTerms a b :: Diff ListableSyntax (Record '[]) (Record '[]) in
        beforeTerm diff `shouldBe` Just a

  describe "afterTerm" $ do
    prop "recovers the after term" $
      \ a b -> let diff = diffTerms a b :: Diff ListableSyntax (Record '[]) (Record '[]) in
        afterTerm diff `shouldBe` Just b

  prop "forward permutations are changes" $ pendingWith "https://github.com/github/semantic-diff/issues/1359"
    -- \ a -> let wrap = termIn Nil . inj
    --            b = wrap [a]
    --            c = wrap [a, b] in
    --   diffTerms (wrap [a, b, c]) (wrap [c, a, b :: Term ListableSyntax (Record '[])]) `shouldBe` merge (Nil, Nil) (inj [ inserting c, merging a, merging b, deleting c ])

  prop "backward permutations are changes" $
    \ a -> let wrap = termIn Nil . inj
               b = wrap [a]
               c = wrap [a, b] in
      diffTerms (wrap [a, b, c]) (wrap [b, c, a :: Term ListableSyntax (Record '[])]) `shouldBe` merge (Nil, Nil) (inj [ deleting a, merging b, merging c, inserting a ])

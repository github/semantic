{-# LANGUAGE DataKinds #-}
module DiffSpec where

import Category
import Data.Functor.Both
import Data.Functor.Foldable (cata)
import Data.Functor.Listable ()
import Data.Record
import RWS
import Diff
import Info
import Interpreter
import Syntax
import Term
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  let decorate = defaultFeatureVectorDecorator (category . termAnnotation)
  prop "equality is reflexive" $
    \ diff -> diff `shouldBe` (diff :: Diff Syntax (Record '[Category]) (Record '[Category]))

  prop "equal terms produce identity diffs" $
    \ a -> let term = decorate (a :: Term Syntax (Record '[Category])) in
      diffCost (diffTerms term term) `shouldBe` 0

  describe "beforeTerm" $ do
    prop "recovers the before term" $
      \ a b -> let diff = diffTerms a b :: Diff Syntax (Record '[Category]) (Record '[Category]) in
        beforeTerm diff `shouldBe` Just a

  describe "afterTerm" $ do
    prop "recovers the after term" $
      \ a b -> let diff = diffTerms a b :: Diff Syntax (Record '[Category]) (Record '[Category]) in
        afterTerm diff `shouldBe` Just b

  prop "forward permutations are changes" $
    \ a -> let wrap = termIn (Program :. Nil) . Indexed
               b = wrap [a]
               c = wrap [a, b] in
      diffTerms (wrap [a, b, c]) (wrap [c, a, b :: Term Syntax (Record '[Category])]) `shouldBe` merge (Program :. Nil, Program :. Nil) (Indexed [ inserting c, mergeTerm a, mergeTerm b, deleting c ])

  prop "backward permutations are changes" $
    \ a -> let wrap = termIn (Program :. Nil) . Indexed
               b = wrap [a]
               c = wrap [a, b] in
      diffTerms (wrap [a, b, c]) (wrap [b, c, a :: Term Syntax (Record '[Category])]) `shouldBe` merge (Program :. Nil, Program :. Nil) (Indexed [ deleting a, mergeTerm b, mergeTerm c, inserting a ])

mergeTerm :: Functor syntax => Term syntax ann -> Diff syntax ann ann
mergeTerm = cata (\ (In ann syntax) -> merge (ann, ann) syntax)

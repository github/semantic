{-# LANGUAGE DataKinds #-}
module DiffSpec where

import Category
import Data.Functor.Both
import Data.Functor.Listable ()
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Statement as Statement
import Data.Record
import Data.Union
import Diff
import Info
import Interpreter
import RWS
import Term
import Test.Hspec
import Test.Hspec.LeanCheck

type Syntax = Union
  '[ Comment.Comment
   , Declaration.Function
   , Declaration.Method
   , Statement.If
   , Syntax.Context
   , Syntax.Empty
   , Syntax.Identifier
   , []
   ]

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

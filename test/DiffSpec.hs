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

{-# LANGUAGE DataKinds #-}
module DiffSpec where

import Data.Diff
import Data.Functor.Listable (ListableSyntax)
import Data.Record
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  prop "equality is reflexive" $
    \ diff -> diff `shouldBe` (diff :: Diff ListableSyntax (Record '[]) (Record '[]))

module Data.Semigroup.App.Spec where

import SpecHelpers
import Data.Semigroup.App

import Test.Tasty
import Test.Tasty.LeanCheck

spec :: TestTree
spec = testGroup "Data.Semigroup.App"
  $ testProperty "App/associative" prop_app_associative
  : testProperty "Merge/associative" prop_merge_associative
  : testProperty "Merge/identity-left" prop_identity_left
  : testProperty "Merge/identity-right" prop_identity_right
  : []

prop_app_associative a b c = a <> (b <> c) == (a <> b) <> (c :: App Maybe Integer)
prop_merge_associative a b c = a <> (b <> c) == (a <> b) <> (c :: AppMerge Maybe String)
prop_identity_left a = mempty <> a == (a :: AppMerge Maybe String)
prop_identity_right a = a <> mempty == (a :: AppMerge Maybe String)

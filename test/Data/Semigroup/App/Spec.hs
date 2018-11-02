module Data.Semigroup.App.Spec where

import SpecHelpers
import Data.Semigroup.App

spec :: Spec
spec = do
  describe "App" $
    prop "should be associative" $
      \a b c -> a <> (b <> c) == (a <> b) <> (c :: App Maybe Integer)

  describe "AppMerge" $ do
    prop "should be associative" $
      \ a b c -> a <> (b <> c) == (a <> b) <> (c :: AppMerge Maybe String)

    prop "identity/left" $
      \ a -> mempty <> a == (a :: AppMerge Maybe String)

    prop "identity/right" $
      \ a -> a <> mempty == (a :: AppMerge Maybe String)

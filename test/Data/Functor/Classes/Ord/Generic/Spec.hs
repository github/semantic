module Data.Functor.Classes.Ord.Generic.Spec where

import Data.Functor.Classes.Ord.Generic
import Data.Functor.Listable
import GHC.Generics
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "genericLiftCompare" $ do
    prop "equivalent to derived compare for product types" $
      \ a b -> genericLiftCompare compare a b `shouldBe` compare a (b :: Product Int)

data Product a = Product a a a
  deriving (Eq, Generic1, Ord, Show)

instance Listable a => Listable (Product a) where
  tiers = cons3 Product

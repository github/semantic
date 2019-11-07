{-# LANGUAGE DeriveGeneric #-}
module Data.Functor.Classes.Generic.Spec (spec) where

import Data.Functor.Classes.Generic
import Data.Functor.Listable
import GHC.Generics
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = do
  describe "Eq1" $ do
    describe "genericLiftEq" $ do
      prop "equivalent to derived (==) for product types" $
        \ a b -> genericLiftEq (==) a b `shouldBe` a == (b :: Product Int)

      prop "equivalent to derived (==) for sum types" $
        \ a b -> genericLiftEq (==) a b `shouldBe` a == (b :: Sum Int)

      prop "equivalent to derived (==) for recursive types" $
        \ a b -> genericLiftEq (==) a b `shouldBe` a == (b :: Tree Int)

  describe "Ord1" $ do
    describe "genericLiftCompare" $ do
      prop "equivalent to derived compare for product types" $
        \ a b -> genericLiftCompare compare a b `shouldBe` compare a (b :: Product Int)

      prop "equivalent to derived compare for sum types" $
        \ a b -> genericLiftCompare compare a b `shouldBe` compare a (b :: Sum Int)

      prop "equivalent to derived compare for recursive types" $
        \ a b -> genericLiftCompare compare a b `shouldBe` compare a (b :: Tree Int)

  describe "Show1" $ do
    describe "genericLiftShowsPrec" $ do
      prop "equivalent to derived showsPrec for product types" $
        \ a -> genericLiftShowsPrec showsPrec showList 0 a "" `shouldBe` showsPrec 0 (a :: Product Int) ""

      prop "equivalent to derived showsPrec for sum types" $
        \ a -> genericLiftShowsPrec showsPrec showList 0 a "" `shouldBe` showsPrec 0 (a :: Sum Int) ""

      prop "equivalent to derived showsPrec for recursive types" $
        \ a -> genericLiftShowsPrec showsPrec showList 0 a "" `shouldBe` showsPrec 0 (a :: Tree Int) ""

      prop "equivalent to derived showsPrec for record selectors" $
        \ a -> genericLiftShowsPrecWithOptions defaultGShow1Options { optionsUseRecordSyntax = True } showsPrec showList 0 a "" `shouldBe` showsPrec 0 (a :: Record Int) ""

      prop "equivalent to derived showsPrec for infix constructors" $
        \ a -> genericLiftShowsPrec showsPrec showList 0 a "" `shouldBe` showsPrec 0 (a :: Infix Int) ""


data Product a = Product a a a
  deriving (Eq, Generic1, Ord, Show)

instance Listable a => Listable (Product a) where
  tiers = cons3 Product


data Sum a = Sum1 a | Sum2 a | Sum3 a
  deriving (Eq, Generic1, Ord, Show)

instance Listable a => Listable (Sum a) where
  tiers = cons1 Sum1 \/ cons1 Sum2 \/ cons1 Sum3


data Tree a = Leaf a | Branch [Tree a]
  deriving (Eq, Generic1, Ord, Show)

instance Listable a => Listable (Tree a) where
  tiers = cons1 Leaf \/ cons1 Branch

instance Eq1 Tree where liftEq = genericLiftEq
instance Ord1 Tree where liftCompare = genericLiftCompare
instance Show1 Tree where liftShowsPrec = genericLiftShowsPrec


data Record a = Record { recordSelector1 :: a, recordSelector2 :: a, recordSelector3 :: a }
  deriving (Eq, Generic1, Ord, Show)

instance Listable a => Listable (Record a) where
  tiers = cons3 Record


data Infix a = a :<>: a
  deriving (Eq, Generic1, Ord, Show)

instance Listable a => Listable (Infix a) where
  tiers = cons2 (:<>:)

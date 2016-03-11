module Data.Adjoined.Spec where

import Data.Adjoined
import Data.Coalescent
import Data.Typeable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop "equality is reflexive" $
    \ a -> a `shouldBe` (a :: Adjoined (Uncoalesced Char))

  monoid (arbitrary :: Gen (Adjoined (Coalesced String)))
  monoid (arbitrary :: Gen (Adjoined (Uncoalesced String)))
  monoid (arbitrary :: Gen (Adjoined (Semicoalesced String)))

monoid :: (Arbitrary a, Coalescent a, Eq a, Show a, Typeable a) => Gen (Adjoined a) -> Spec
monoid gen =
  describe ("Monoid (" ++ showTypeOf (`asGeneratedTypeOf` gen) ++ ")") $ do
    prop "mempty is the left identity" $ forAll gen $
      \ a -> mempty `mappend` a `shouldBe` a

    prop "mempty is the right identity" $ forAll gen $
      \ a -> a `mappend` mempty `shouldBe` a

    prop "mappend is associative" $ forAll gen $
      \ a b c -> (a `mappend` b) `mappend` c `shouldBe` a `mappend` (b `mappend` c)


instance Arbitrary a => Arbitrary (Adjoined a) where
  arbitrary = fromList <$> arbitrary


-- | A wrapper which never coalesces values.
newtype Uncoalesced a = Uncoalesced { runUncoalesced :: a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Uncoalesced a) where
  arbitrary = Uncoalesced <$> arbitrary

instance Coalescent (Uncoalesced a) where
  coalesce _ _ = Nothing


-- | A wrapper which always coalesces values.
newtype Coalesced a = Coalesced { runCoalesced :: a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Coalesced a) where
  arbitrary = Coalesced <$> arbitrary

instance Monoid a => Coalescent (Coalesced a) where
  coalesce a b = Just (Coalesced (runCoalesced a `mappend` runCoalesced b))


-- | A wrapper which only coalesces some values.
newtype Semicoalesced a = Semicoalesced { runSemicoalesced :: (Bool, a) }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Semicoalesced a) where
  arbitrary = Semicoalesced <$> arbitrary

instance Monoid a => Coalescent (Semicoalesced a) where
  Semicoalesced (True, a) `coalesce` Semicoalesced (flag, b) = Just (Semicoalesced (flag, a `mappend` b))
  Semicoalesced (False, _) `coalesce` _ = Nothing


-- | Returns a string with the name of a type.
-- |
-- | Use with `asTypeOf` or `asGeneratedTypeOf` to show type names for parameters without fighting type variable scoping:
-- |
-- |   showTypeOf (`asTypeOf` someTypeParametricValue)
showTypeOf :: Typeable a => (a -> a) -> String
showTypeOf f = show (typeRep (proxyOf f))
  where proxyOf :: (a -> a) -> Proxy a
        proxyOf _ = Proxy

-- | Type-restricted `const`, usually written infix or as an operator section with `showTypeOf`.
asGeneratedTypeOf :: a -> Gen a -> a
asGeneratedTypeOf = const

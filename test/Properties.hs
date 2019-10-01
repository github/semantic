-- | Defines useful Hedgehog checkers based around the properties of
-- standard typeclasses (associativity, reflexivity, etc.) This module
-- is similar to the hedgehog-checkers package on Hackage, but said
-- package doesn't work with hedgehog-1.0, so we reproduce these
-- (largely-trivial) functions here.

module Properties
  ( associative
  , monoidal
  ) where

import Hedgehog
import GHC.Stack

associative :: (Eq a, Show a) => (a -> a -> a) -> Gen a -> Property
associative fn gen = property . withFrozenCallStack $ do
  (a, b, c) <- forAll ((,,) <$> gen <*> gen <*> gen)
  fn a (fn b c) === fn (fn a b) c

monoidal :: (Eq a, Show a, Monoid a) => Gen a -> Property
monoidal gen = property . withFrozenCallStack $ do
  it <- forAll gen
  mempty <> it === it
  it <> mempty === it

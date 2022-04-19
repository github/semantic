{-# LANGUAGE OverloadedStrings #-}
module Range.Test
( testTree
) where

import           Control.Monad (join)
import           Hedgehog hiding (Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Source.Range
import qualified Test.Tasty as Tasty
import           Test.Tasty.Hedgehog (testPropertyNamed)

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Source.Range"
  [ Tasty.testGroup "Semigroup"
    [ testPropertyNamed "associativity" "Range_associativity" . property $ do
      (a, b, c) <- forAll ((,,) <$> range <*> range <*> range)
      a <> (b <> c) === (a <> b) <> c
    ]
  ]


range :: MonadGen m => m Range
range = Gen.choice [ empty, nonEmpty ] where
  point    = Gen.int (Range.linear 0 100)
  empty    = join Range <$> point
  nonEmpty = do
    start <- point
    length <- point
    pure $! Range start (start + length + 1)

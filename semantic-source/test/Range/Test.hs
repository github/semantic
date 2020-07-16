module Range.Test
( testTree
) where

import           Control.Monad (join)
import           Hedgehog hiding (Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Source.Range
import qualified Test.Tasty as Tasty

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Source.Range"
  []


range :: MonadGen m => m Range
range = Gen.choice [ empty, nonEmpty ] where
  point    = Gen.int (Range.linear 0 100)
  empty    = join Range <$> point
  nonEmpty = do
    start <- point
    length <- point
    pure $! Range start (start + length + 1)

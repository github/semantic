module Data.Semigroup.App.Spec (testTree) where

import           Data.Semigroup.App
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Properties
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Tasty

app :: MonadGen m => m (App Maybe Integer)
app = App <$> Gen.maybe (Gen.integral (Range.linear 0 10000))

merge :: MonadGen m => m (AppMerge Maybe String)
merge = AppMerge <$> Gen.maybe (Gen.string (Range.linear 0 10) Gen.ascii)

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Data.Semigroup.App"
  [ Tasty.testGroup "App"
    [ Tasty.testProperty "is associative" (associative (<>) app)
    ]
  , Tasty.testGroup "AppMerge"
    [ Tasty.testProperty "is associative" (associative (<>) merge)
    , Tasty.testProperty "is monoidal" (monoidal merge)
    ]
  ]

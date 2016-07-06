module Diff.Arbitrary where

import Diff
import Data.Bifunctor.Join
import Data.Bifunctor.Join.Arbitrary ()
import Data.Functor.Foldable (unfold)
import qualified Data.OrderedMap as Map
import Patch
import Patch.Arbitrary ()
import Syntax
import Prologue
import Term.Arbitrary
import Test.QuickCheck hiding (Fixed)

data ArbitraryDiff leaf annotation
  = ArbitraryFree (Join (,) annotation) (Syntax leaf (ArbitraryDiff leaf annotation))
  | ArbitraryPure (Patch (ArbitraryTerm leaf annotation))
  deriving (Show, Eq, Generic)

unArbitraryDiff :: ArbitraryDiff leaf annotation -> FreeF (CofreeF (Syntax leaf) (Join (,) annotation)) (Patch (ArbitraryTerm leaf annotation)) (ArbitraryDiff leaf annotation)
unArbitraryDiff (ArbitraryFree a s) = Free (a :< s)
unArbitraryDiff (ArbitraryPure p) = Pure p

toDiff :: ArbitraryDiff leaf annotation -> Diff leaf annotation
toDiff = fmap (fmap toTerm) . unfold unArbitraryDiff

diffOfSize :: (Arbitrary leaf, Arbitrary annotation) => Int -> Gen (ArbitraryDiff leaf annotation)
diffOfSize n
  | n <= 0 = ArbitraryFree <$> arbitrary <*> syntaxOfSize diffOfSize n
  | otherwise = oneof
    [ ArbitraryFree <$> arbitrary <*> syntaxOfSize diffOfSize n
    , ArbitraryPure <$> patchOfSize n ]
  where patchOfSize 1 = oneof [ Insert <$> termOfSize 1
                              , Delete <$> termOfSize 1 ]
        patchOfSize n = do
          m <- choose (1, n - 1)
          left <- termOfSize m
          right <- termOfSize (n - m)
          oneof [ Insert <$> termOfSize n
                , Delete <$> termOfSize n
                , pure (Replace left right) ]

arbitraryDiffSize :: ArbitraryDiff leaf annotation -> Int
arbitraryDiffSize = iter (succ . sum) . fmap (sum . fmap (arbitraryTermSize . unfold runCofree)) . toDiff


-- Instances

instance (Eq leaf, Eq annotation, Arbitrary leaf, Arbitrary annotation) => Arbitrary (ArbitraryDiff leaf annotation) where
  arbitrary = sized $ \ n -> do
    m <- choose (0, n)
    diffOfSize m
  shrink = genericShrink

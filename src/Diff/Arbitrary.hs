{-# LANGUAGE TypeFamilies #-}
module Diff.Arbitrary where

import Diff
import Data.Bifunctor.Join
import Data.Bifunctor.Join.Arbitrary ()
import Data.Functor.Foldable (Base, cata, unfold, Unfoldable(embed))
import qualified Data.List as List
import qualified Data.OrderedMap as Map
import Patch
import Patch.Arbitrary
import Syntax
import Prologue
import Term
import Term.Arbitrary
import Test.QuickCheck hiding (Fixed)

newtype ArbitraryDiff leaf annotation = ArbitraryDiff { unArbitraryDiff :: FreeF (CofreeF (Syntax leaf) (Join (,) annotation)) (Patch (ArbitraryTerm leaf annotation)) (ArbitraryDiff leaf annotation) }
  deriving (Show, Eq, Generic)

toDiff :: ArbitraryDiff leaf annotation -> Diff leaf annotation
toDiff = fmap (fmap toTerm) . unfold unArbitraryDiff

diffOfSize :: (Arbitrary leaf, Arbitrary annotation) => Int -> Gen (ArbitraryDiff leaf annotation)
diffOfSize n = oneof
  [ (ArbitraryDiff .) . (Free .) . (:<) <$> arbitrary <*> syntaxOfSize n
  , ArbitraryDiff . Pure <$> patchOf (termOfSize n) ]
  where syntaxOfSize n | n <= 1 = oneof $ (Leaf <$> arbitrary) : branchGeneratorsOfSize n
                       | otherwise = oneof $ branchGeneratorsOfSize n
        branchGeneratorsOfSize n =
          [ Indexed <$> childrenOfSize (pred n)
          , Fixed <$> childrenOfSize (pred n)
          , (Keyed .) . (Map.fromList .) . zip <$> infiniteListOf arbitrary <*> childrenOfSize (pred n)
          ]
        childrenOfSize n | n <= 0 = pure []
        childrenOfSize n = do
          m <- choose (1, n)
          first <- diffOfSize m
          rest <- childrenOfSize (n - m)
          pure $! first : rest

arbitraryDiffSize :: ArbitraryDiff leaf annotation -> Int
arbitraryDiffSize = cata (succ . sum) . fmap (fmap (arbitraryTermSize . unfold runCofree)) . toDiff


-- Instances

type instance Base (ArbitraryTerm leaf annotation) = CofreeF (Syntax leaf) annotation
instance Unfoldable (ArbitraryTerm leaf annotation) where embed = ArbitraryTerm

instance (Eq leaf, Eq annotation, Arbitrary leaf, Arbitrary annotation) => Arbitrary (ArbitraryDiff leaf annotation) where
  arbitrary = sized $ \ n -> do
    m <- choose (0, n)
    diffOfSize m

  shrink diff@(ArbitraryDiff annotated) = case annotated of
    Free (annotation :< syntax) -> (subterms diff ++) $ filter (/= diff) $
      (ArbitraryDiff .) . (Free .) . (:<) <$> shrink annotation <*> case syntax of
        Leaf a -> Leaf <$> shrink a
        Indexed i -> Indexed <$> (List.subsequences i >>= recursivelyShrink)
        Fixed f -> Fixed <$> (List.subsequences f >>= recursivelyShrink)
        Keyed k -> Keyed . Map.fromList <$> (List.subsequences (Map.toList k) >>= recursivelyShrink)
    Pure patch -> ArbitraryDiff . Pure <$> shrink patch

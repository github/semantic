module Diff.Arbitrary where

import Diff
import Data.Bifunctor.Join
import Data.Bifunctor.Join.Arbitrary ()
import Data.Functor.Foldable (unfold)
import qualified Data.List as List
import qualified Data.OrderedMap as Map
import Patch
import Patch.Arbitrary ()
import Syntax
import Prologue
import Term.Arbitrary
import Test.QuickCheck hiding (Fixed)

newtype ArbitraryDiff leaf annotation = ArbitraryDiff { unArbitraryDiff :: FreeF (CofreeF (Syntax leaf) (Join (,) annotation)) (Patch (ArbitraryTerm leaf annotation)) (ArbitraryDiff leaf annotation) }
  deriving (Show, Eq, Generic)

toDiff :: ArbitraryDiff leaf annotation -> Diff leaf annotation
toDiff = fmap (fmap toTerm) . unfold unArbitraryDiff

diffOfSize :: (Arbitrary leaf, Arbitrary annotation) => Int -> Gen (ArbitraryDiff leaf annotation)
diffOfSize n = (ArbitraryDiff .) . (Free .) . (:<) <$> arbitrary <*> syntaxOfSize n
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


-- Instances

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
    -- Pure patch -> pure . Pure <$> shrink patch


-- Instances

-- instance (Eq leaf, Eq annotation, Arbitrary leaf, Arbitrary annotation) => Arbitrary (ArbitraryDiff leaf annotation) where
--   arbitrary = scale (`div` 2) $ sized (\ x -> boundedTerm x x) -- first indicates the cube of the max length of lists, second indicates the cube of the max depth of the tree
--     where boundedTerm maxLength maxDepth = oneof [ (ArbitraryDiff .) . (Free .) . (:<) <$> (pure <$> arbitrary) <*> boundedSyntax maxLength maxDepth
--                                                  , ArbitraryDiff . Pure <$> arbitrary ]
--           boundedSyntax _ maxDepth | maxDepth <= 0 = Leaf <$> arbitrary
--           boundedSyntax maxLength maxDepth = frequency
--             [ (12, Leaf <$> arbitrary),
--               (1, Indexed . take maxLength <$> listOf (smallerTerm maxLength maxDepth)),
--               (1, Fixed . take maxLength <$> listOf (smallerTerm maxLength maxDepth)),
--               (1, Keyed . Map.fromList . take maxLength <$> listOf (arbitrary >>= (\x -> (,) x <$> smallerTerm maxLength maxDepth))) ]
--           smallerTerm maxLength maxDepth = boundedTerm (div maxLength 3) (div maxDepth 3)
--
--   shrink (ArbitraryDiff diff) = case diff of
--     Free (annotation :< syntax) -> (subterms (ArbitraryDiff diff) ++) $ filter (/= ArbitraryDiff diff) $
--       (ArbitraryDiff .) . (Free .) . (:<) <$> traverse shrink annotation <*> case syntax of
--         Leaf a -> Leaf <$> shrink a
--         Indexed i -> Indexed <$> (List.subsequences i >>= recursivelyShrink)
--         Fixed f -> Fixed <$> (List.subsequences f >>= recursivelyShrink)
--         Keyed k -> Keyed . Map.fromList <$> (List.subsequences (Map.toList k) >>= recursivelyShrink)
--     Pure patch -> ArbitraryDiff . Pure <$> shrink patch

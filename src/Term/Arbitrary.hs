module Term.Arbitrary where

import Data.Functor.Foldable (unfold)
import qualified Data.List as List
import qualified Data.OrderedMap as Map
import Data.Text.Arbitrary ()
import Prologue
import Syntax
import Term
import Test.QuickCheck hiding (Fixed)

newtype ArbitraryTerm leaf annotation = ArbitraryTerm { unArbitraryTerm :: TermF leaf annotation (ArbitraryTerm leaf annotation) }
  deriving (Show, Eq, Generic)

toTerm :: ArbitraryTerm leaf annotation -> Term leaf annotation
toTerm = unfold unArbitraryTerm

termOfSize :: (Arbitrary leaf, Arbitrary annotation) => Int -> Gen (ArbitraryTerm leaf annotation)
termOfSize n = (ArbitraryTerm .) . (:<) <$> arbitrary <*> syntaxOfSize n
  where syntaxOfSize n = oneof
          [ Leaf <$> arbitrary
          , Indexed <$> childrenOfSize (pred n)
          , Fixed <$> childrenOfSize (pred n)
          , (Keyed .) . (Map.fromList .) . zip <$> infiniteListOf arbitrary <*> childrenOfSize (pred n)
          ]
        childrenOfSize 0 = pure []
        childrenOfSize n = do
          m <- choose (1, n)
          first <- termOfSize m
          rest <- childrenOfSize (n - m)
          pure $! first : rest

instance (Eq leaf, Eq annotation, Arbitrary leaf, Arbitrary annotation) => Arbitrary (ArbitraryTerm leaf annotation) where
  arbitrary = sized termOfSize

  shrink term@(ArbitraryTerm (annotation :< syntax)) = (subterms term ++) $ filter (/= term) $
    (ArbitraryTerm .) . (:<) <$> shrink annotation <*> case syntax of
      Leaf a -> Leaf <$> shrink a
      Indexed i -> Indexed <$> (List.subsequences i >>= recursivelyShrink)
      Fixed f -> Fixed <$> (List.subsequences f >>= recursivelyShrink)
      Keyed k -> Keyed . Map.fromList <$> (List.subsequences (Map.toList k) >>= recursivelyShrink)

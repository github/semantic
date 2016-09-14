{-# LANGUAGE TypeFamilies #-}
module Term.Arbitrary where

import Data.Functor.Foldable (Base, cata, unfold, Corecursive(embed))
import Data.Text.Arbitrary ()
import Prologue
import Syntax
import Term
import Test.QuickCheck hiding (Fixed)

data ArbitraryTerm leaf annotation = ArbitraryTerm { annotation :: annotation, syntax :: Syntax leaf (ArbitraryTerm leaf annotation)}
  deriving (Show, Eq, Generic)

unArbitraryTerm :: ArbitraryTerm leaf annotation -> TermF (Syntax leaf) annotation (ArbitraryTerm leaf annotation)
unArbitraryTerm (ArbitraryTerm a s) = a :< s

toTerm :: ArbitraryTerm leaf annotation -> Term (Syntax leaf) annotation
toTerm = unfold unArbitraryTerm

termOfSize :: (Arbitrary leaf, Arbitrary annotation) => Int -> Gen (ArbitraryTerm leaf annotation)
termOfSize n = ArbitraryTerm <$> arbitrary <*> syntaxOfSize termOfSize n

arbitraryTermSize :: ArbitraryTerm leaf annotation -> Int
arbitraryTermSize = cata (succ . sum) . toTerm


-- Instances

<<<<<<< HEAD
type instance Base (ArbitraryTerm leaf annotation) = CofreeF (Syntax leaf) annotation
instance Corecursive (ArbitraryTerm leaf annotation) where embed = ArbitraryTerm
=======
type instance Base (ArbitraryTerm leaf annotation) = TermF (Syntax leaf) annotation
instance Unfoldable (ArbitraryTerm leaf annotation) where embed (a :< s) = ArbitraryTerm a s
>>>>>>> master

instance (Eq leaf, Eq annotation, Arbitrary leaf, Arbitrary annotation) => Arbitrary (ArbitraryTerm leaf annotation) where
  arbitrary = sized $ \ n -> do
    m <- choose (0, n)
    termOfSize m

  shrink = genericShrink

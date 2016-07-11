module Syntax where

import Prologue
import Data.OrderedMap as Map
import Data.Text.Arbitrary ()
import GHC.Generics
import qualified Data.Text as T
import Test.QuickCheck hiding (Fixed)

-- | A node in an abstract syntax tree.
data Syntax
  a -- ^ The type of leaves in the syntax tree, typically String, but possibly some datatype representing different leaves more precisely.
  f -- ^ The type representing another level of the tree, e.g. the children of branches. Often Cofree or Fix or similar.
  =
  -- | A terminal syntax node, e.g. an identifier, or atomic literal.
  Leaf a
  -- | An ordered branch of child nodes, expected to be variadic in the grammar, e.g. a list of statements or uncurried function parameters.
  | Indexed [f]
  -- | An ordered branch of child nodes, expected to be of fixed length in the grammar, e.g. a binary operator & its operands.
  | Fixed [f]
  -- | A branch of child nodes indexed by some String identity. This is useful for identifying e.g. methods & properties in a class scope by their names. Note that comments can generally occur in these scopes as well; one strategy for dealing with this is to identify comments by their text in the source.
  | Keyed (OrderedMap T.Text f)
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)


-- Instances

syntaxOfSize :: Arbitrary leaf => (Int -> Gen f) -> Int -> Gen (Syntax leaf f)
syntaxOfSize recur n | n <= 1 = oneof $ (Leaf <$> arbitrary) : branchGeneratorsOfSize n
                     | otherwise = oneof $ branchGeneratorsOfSize n
  where branchGeneratorsOfSize n =
          [ Indexed <$> childrenOfSize (pred n)
          , Fixed <$> childrenOfSize (pred n)
          , (Keyed .) . (Map.fromList .) . zip <$> infiniteListOf arbitrary <*> childrenOfSize (pred n)
          ]
        childrenOfSize n | n <= 0 = pure []
        childrenOfSize n = do
          m <- choose (1, n)
          first <- recur m
          rest <- childrenOfSize (n - m)
          pure $! first : rest

instance (Arbitrary leaf, Arbitrary f) => Arbitrary (Syntax leaf f) where
  arbitrary = sized (syntaxOfSize (`resize` arbitrary) )

  shrink = genericShrink

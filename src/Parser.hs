module Parser where

import Category
import Diff
import Range
import Syntax
import Term
import Control.Comonad.Cofree
import qualified Data.OrderedMap as Map
import qualified Data.Set as Set
import Source
import Data.Text as Text

-- | A function that takes a source file and returns an annotated AST.
-- | The return is in the IO monad because some of the parsers are written in C
-- | and aren't pure.
type Parser = Source Char -> IO (Term Text Info)

-- | Given a source string, the term's range, production name, and
-- | production/child pairs, construct the term.
type Constructor = Source Char -> Range -> String -> [Term Text Info] -> Term Text Info

-- | Categories that are treated as keyed nodes.
keyedCategories :: Set.Set Category
keyedCategories = Set.fromList [ DictionaryLiteral ]

-- | Categories that are treated as fixed nodes.
fixedCategories :: Set.Set Category
fixedCategories = Set.fromList [ BinaryOperator, Pair ]

-- | Given a function that maps production names to sets of categories, produce
-- | a Constructor.
termConstructor :: (String -> Set.Set Category) -> Constructor
termConstructor mapping source range name = (Info range categories :<) . construct
  where
    categories = mapping name
    construct [] = Leaf . pack . toList $ slice range source
    construct children | categories `intersect` fixedCategories = Fixed children
    construct children | categories `intersect` keyedCategories = Keyed . Map.fromList $ assignKey <$> children
    construct children = Indexed children
    intersect a b = not . Set.null $ Set.intersection a b
    assignKey node@(Info _ categories :< Fixed (key : _)) | Set.member Pair categories = (getSubstring key, node)
    assignKey node = (getSubstring node, node)
    getSubstring (Info range _ :< _) = pack . toList $ slice range source

module Parser where

import Prologue hiding (Constructor)
import Data.String
import Data.Text (pack)
import Category
import Info
import Range
import Syntax
import Term
import qualified Data.OrderedMap as Map
import qualified Data.Set as Set
import Source

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

-- | Should these categories be treated as keyed nodes?
isKeyed :: Category -> Bool
isKeyed = flip Set.member keyedCategories

-- | Should these categories be treated as fixed nodes?
isFixed :: Category -> Bool
isFixed = flip Set.member fixedCategories

-- | Given a function that maps production names to sets of categories, produce
-- | a Constructor.
termConstructor :: (String -> Category) -> Constructor
termConstructor mapping source range name children = cofree (Info range categories (1 + sum (size . extract <$> children)) :< construct children)
  where
    categories = mapping name
    construct :: [Term Text Info] -> Syntax Text (Term Text Info)
    construct [] = Leaf . pack . toString $ slice range source
    construct children | isFixed categories = Fixed children
    construct children | isKeyed categories = Keyed . Map.fromList $ assignKey <$> children
    construct children = Indexed children
    assignKey node | Info _ category _ :< Fixed (key : _) <- runCofree node, Pair == category = (getSubstring key, node)
    assignKey node = (getSubstring node, node)
    getSubstring term = pack . toString $ slice (characterRange (extract term)) source

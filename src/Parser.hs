module Parser where

import Category
import Diff
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

-- | Given a source string and a term’s annotation & production/child pairs, construct the term.
type Constructor = Source Char -> Info -> [(String, Term Text Info)] -> Term Text Info

-- | Categories that are treated as keyed nodes.
keyedCategories :: Set.Set Category
keyedCategories = Set.fromList [ DictionaryLiteral ]

-- | Categories that are treated as fixed nodes.
fixedCategories :: Set.Set Category
fixedCategories = Set.fromList [ BinaryOperator ]

-- | Given two sets of production names, produce a Constructor.
termConstructor :: Constructor
termConstructor source info@(Info range categories) = (info :<) . construct
  where construct [] = Leaf . pack . toList $ slice range source
        construct children | categories `intersect` fixedCategories = Fixed $ fmap snd children
        construct children | categories `intersect` keyedCategories = Keyed . Map.fromList $ assignKey <$> children
        construct children = Indexed $ snd <$> children
        intersect a b = not . Set.null $ Set.intersection a b
        assignKey ("pair", node@(_ :< Fixed (key : _))) = (getSubstring key, node)
        assignKey (_, node) = (getSubstring node, node)
        getSubstring (Info range _ :< _) = pack . toList $ slice range source

module Parser where

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

-- | Given two sets of production names, produce a Constructor.
constructorForProductions :: Set.Set String -> Set.Set String -> Constructor
constructorForProductions keyed fixed source info@(Info range categories) = (info :<) . construct
  where construct [] = Leaf . pack . toList $ slice range source
        construct children | not . Set.null $ Set.intersection fixed categories = Fixed $ fmap snd children
        construct children | not . Set.null $ Set.intersection keyed categories = Keyed . Map.fromList $ assignKey <$> children
        construct children = Indexed $ snd <$> children
        assignKey ("pair", node@(_ :< Fixed (key : _))) = (getSubstring key, node)
        assignKey (_, node) = (getSubstring node, node)
        getSubstring (Info range _ :< _) = pack . toList $ slice range source

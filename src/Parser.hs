module Parser where

import Diff
import Range
import Syntax
import Term
import Control.Comonad.Cofree
import qualified OrderedMap as Map
import qualified Data.Set as Set
import qualified Data.Text as T

type Parser = T.Text -> IO (Term T.Text Info)

-- | Given a source string and a term’s annotation & production/child pairs, construct the term.
type Constructor = T.Text -> Info -> [(T.Text, Term T.Text Info)] -> Term T.Text Info

-- | Given two sets of production names, produce a Constructor.
constructorForProductions :: Set.Set T.Text -> Set.Set T.Text -> Constructor
constructorForProductions keyed fixed source info@(Info range categories) = (info :<) . construct
  where construct [] = Leaf (substring range source)
        construct children | not . Set.null $ Set.intersection fixed categories = Fixed $ fmap snd children
        construct children | not . Set.null $ Set.intersection keyed categories = Keyed . Map.fromList $ assignKey <$> children
        construct children = Indexed $ fmap snd children
        assignKey ("pair", node@(_ :< Fixed (key : _))) = (getSubstring key, node)
        assignKey (_, node) = (getSubstring node, node)
        getSubstring (Info range _ :< _) = substring range source

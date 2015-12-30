module Parser where

import Diff
import Syntax
import Term
import Control.Comonad.Cofree
import qualified OrderedMap as Map
import qualified Data.Set as Set
import Source
import Data.Text as Text

type Parser = Source Char -> IO (Term Text Info)

-- | Given a source string and a term’s annotation & production/child pairs, construct the term.
type Constructor = Source Char -> Info -> [(Text, Term Text Info)] -> Term Text Info

-- | Given two sets of production names, produce a Constructor.
constructorForProductions :: Set.Set Text -> Set.Set Text -> Constructor
constructorForProductions keyed fixed source info@(Info range categories) = (info :<) . construct
  where construct [] = Leaf . pack . toList $ slice range source
        construct children | not . Set.null $ Set.intersection fixed categories = Fixed $ fmap snd children
        construct children | not . Set.null $ Set.intersection keyed categories = Keyed . Map.fromList $ assignKey <$> children
        construct children = Indexed $ snd <$> children
        assignKey ("pair", node@(_ :< Fixed (key : _))) = (getSubstring key, node)
        assignKey (_, node) = (getSubstring node, node)
        getSubstring (Info range _ :< _) = pack . toList $ slice range source

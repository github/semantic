module Split where

import Diff
import Patch
import Syntax
import Term
import Range
import Control.Comonad.Cofree
import Control.Monad.Free
import qualified Data.Map as Map
import qualified Data.Set as Set
import Rainbow

type ClassName = String
type Element a = Cofree (Syntax a) (Maybe ClassName, String)

data HTML =
  Text String
  | Span (Maybe ClassName) String
  | Ul (Maybe ClassName) [HTML]
  | Dl (Maybe ClassName) [HTML]
  | Dt String
  deriving (Eq, Show)

split :: Diff a Info -> String -> String -> IO ByteString
split _ _ _ = return mempty

splitDiff :: Diff a Info -> String -> String -> (Maybe (HTML, Range), Maybe (HTML, Range))
splitDiff diff before after = iter toElements $ splitPatch before after <$> diff
  where
    toElements (Annotated (left, right) (Leaf _)) = (Just $ leafToElement before left, Just $ leafToElement after right)

    leafToElement source (Info range _ categories) = (Span (classify categories) $ substring range source, range)

splitPatch :: String -> String -> Patch (Term a Info) -> (Maybe (HTML, Range), Maybe (HTML, Range))
splitPatch before after patch = (splitTerm before <$> Patch.before patch, splitTerm after <$> Patch.after patch)

splitTerm :: String -> Term a Info -> (HTML, Range)
splitTerm source = cata toElement where
  toElement (Info range _ categories) (Leaf _) = (Span (classify categories) $ substring range source, range)
  toElement (Info range _ categories) (Indexed i) = makeList i range categories
  toElement (Info range _ categories) (Fixed i) = makeList i range categories
  toElement (Info range _ categories) (Keyed k) = makeMap (Map.toList k) range categories

  accumulate (children, previous) (child, range) = (children ++ [ subtext previous $ start range, child ], end range)
  accumulateFromMap (children, previous) (key, (child, range)) = (children ++ [ subtext previous $ start range, Dt key, child ], end range)

  makeList i range categories = (Ul (classify categories) items, range)
    where
      (children, previous) = foldl accumulate ([], start range) i
      items = children ++ [ subtext previous $ end range ]

  makeMap k range categories = (Dl (classify categories) items, range)
    where
      (children, previous) = foldl accumulateFromMap ([], start range) k
      items = children ++ [ subtext previous $ end range ]

  subtext :: Int -> Int -> HTML
  subtext start end = Text $ substring (Range start end) source

classify :: Set.Set Category -> Maybe ClassName
classify = foldr (const . Just) Nothing

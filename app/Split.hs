module Split where

import Diff
import Patch
import Syntax
import Term
import Unified
import Control.Comonad.Cofree
import qualified Data.Map as Map
import qualified Data.Set as Set
import Rainbow

type ClassName = String
type Element a = Cofree (Syntax a) (Maybe ClassName, String)

data HTML =
  Text String
  | Span (Maybe ClassName) String
  | Ul (Maybe ClassName) [HTML]
  | Dl (Maybe ClassName) (Map.Map String HTML)
  deriving (Eq, Show)

split :: Diff a Info -> String -> String -> IO ByteString
split _ _ _ = return mempty

splitDiff :: Diff a Info -> String -> String -> [(String, String)]
splitDiff _ _ _ = []

splitPatch :: String -> String -> Patch (Term a Info) -> (Maybe HTML, Maybe HTML)
splitPatch before after patch = (fmap (splitTerm before) $ Patch.before patch, fmap (splitTerm after) $ Patch.after patch)

splitTerm :: String -> Term a Info -> HTML
splitTerm source term = fst $ cata toElement term where
  toElement (Info range lineRange categories) (Leaf _) = (Span (classify categories) $ substring range source, range)
  toElement (Info range lineRange categories) (Indexed i) = (Ul (classify categories) $ children ++ [ subtext previous $ end range ], range) where
    (children, previous) = foldl accumulate ([], start range) i
  accumulate (children, previous) (child, range) = (children ++ [ subtext previous $ start range, child ], end range)
  subtext :: Int -> Int -> HTML
  subtext start end = Text $ substring (Range start end) source

classify :: Set.Set Category -> Maybe ClassName
classify categories = foldr (const . Just) Nothing categories

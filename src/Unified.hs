module Unified where

import Diff
import Patch
import Syntax
import Term
import Control.Arrow
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.List hiding (foldl)
import qualified Data.Map as Map

unified :: Diff a Info -> String -> String -> String
unified diff before after =
  fst $ iter g mapped where
    mapped = fmap (unifiedPatch &&& range) diff
    g (Annotated (_, info) syntax) = f info syntax
    f (Info range _) (Leaf _) = (substring range after, Just range)
    f (Info range _) (Indexed i) = (unifiedRange range i after, Just range)
    f (Info range _) (Fixed f) = (unifiedRange range f after, Just range)
    f (Info range _) (Keyed k) = (unifiedRange range (sort $ snd <$> Map.toList k) after, Just range)

    unifiedPatch :: Patch (Term a Info) -> String
    unifiedPatch _ = ""

    unifiedTerm :: Term a Info -> String -> String
    unifiedTerm term source = fst $ cata f term

    unifiedRange :: Range -> [(String, Maybe Range)] -> String -> String
    unifiedRange range children source = out ++ substring Range { start = previous, end = end range } after where
      (out, previous) = foldl accumulateContext ("", start range) children
      accumulateContext (out, previous) (child, Just range) = (out ++ substring Range { start = previous, end = start range } source ++ child, end range)
      accumulateContext (out, previous) (child, _) = (out ++ child, previous)

substring :: Range -> String -> String
substring range = take (end range) . drop (start range)

range :: Patch (Term a Info) -> Maybe Range
range patch = range . extract <$> after patch where
  extract (annotation :< _) = annotation
  range (Info range _) = range

change :: String -> String -> String
change bound content = "{" ++ bound ++ content ++ bound ++ "}"

instance Ord Range where
  a <= b = start a <= start b

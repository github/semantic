module Unified where

import Diff
import Patch
import Syntax
import Term
import Control.Arrow
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.Map

unified :: Diff a Info -> String -> String -> String
unified diff before after =
  fst $ iter f mapped where
    mapped = fmap (unifiedPatch &&& range) diff
    f (Annotated (_, Info range _) (Leaf _)) = (substring range after, Just range)
    f (Annotated (_, Info range _) (Indexed i)) = (unifiedRange range i after, Just range)
    f (Annotated (_, Info range _) (Fixed f)) = (unifiedRange range f after, Just range)
    f (Annotated (_, Info range _) (Keyed k)) = ("", Just range)

    unifiedPatch :: Patch (Term a annotation) -> String
    unifiedPatch _ = ""

    unifiedRange :: Range -> [(String, Maybe Range)] -> String -> String
    unifiedRange range children source = out ++ substring Range { start = previous, end = end range } after where
      (out, previous) = Prelude.foldl accumulateContext ("", start range) children
      accumulateContext (out, previous) (child, Just range) = (out ++ substring Range { start = previous, end = start range } source ++ child, end range)
      accumulateContext (out, previous) (child, _) = (out ++ child, previous)

substring :: Range -> String -> String
substring range = take (end range) . drop (start range)

range :: Patch (Term a Info) -> Maybe Range
range patch = range . extract <$> after patch where
  extract (annotation :< _) = annotation
  range (Info range _) = range

instance Ord Range where
  a <= b = start a <= start b

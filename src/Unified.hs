module Unified where

import Diff
import Patch
import Syntax
import Term
import Control.Arrow
import Control.Monad.Free
import Control.Comonad.Cofree

unified :: Diff a Info -> String -> String -> String
unified diff before after =
  fst $ iter f mapped where
    mapped = fmap (unifiedPatch &&& range) diff
    f (Annotated (_, Info range _) (Leaf _)) = (substring range after, Just range)
    f (Annotated (_, Info range _) (Indexed i)) = ("", Just range)
    f (Annotated (_, Info range _) (Fixed f)) = ("", Just range)
    f (Annotated (_, Info range _) (Keyed k)) = ("", Just range)

    unifiedPatch :: Patch (Term a annotation) -> String
    unifiedPatch _ = ""
    unifiedRange :: Range -> [(String, Maybe Range)] -> String -> String
    unifiedRange _ _ _ = ""

substring :: Range -> String -> String
substring range = take (end range) . drop (start range)

range :: Patch (Term a Info) -> Maybe Range
range patch = range . extract <$> after patch where
  extract (annotation :< _) = annotation
  range (Info range _) = range

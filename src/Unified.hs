module Unified where

import Diff
import Patch
import Syntax
import Term
import Control.Monad.Free

unified :: Diff a Info -> String -> String -> String
unified diff before after =
  iter f mapped where
    mapped = fmap unifiedPatch diff
    f (Annotated (_, Info range _) (Leaf _)) = substring range after
    f (Annotated annotations (Indexed i)) = ""
    f (Annotated annotations (Fixed f)) = ""
    f (Annotated annotations (Keyed k)) = ""
    unifiedPatch :: Patch (Term a annotation) -> String
    unifiedPatch _ = ""
    unifiedRange :: Range -> [(String, Maybe Range)] -> String -> String
    unifiedRange _ _ _ = ""

substring :: Range -> String -> String
substring range = take (end range) . drop (start range)

-- | Semantic functionality for Python programs.
module Language.Python
( Term(..)
) where

import qualified Language.Python.Tags as PyTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Python.AST as Py

newtype Term a = Term { getTerm :: Py.Module a }

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . PyTags.tags . getTerm

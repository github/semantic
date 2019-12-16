-- | Semantic functionality for Ruby programs.
module Language.Ruby
( Term(..)
, TreeSitter.Ruby.tree_sitter_ruby
) where

import qualified Language.Ruby.Tags as PyTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Ruby (tree_sitter_ruby)
import qualified TreeSitter.Ruby.AST as Rb
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: Rb.Program a }

instance TS.Unmarshal Term where
  unmarshalNode node = Term <$> TS.unmarshalNode node

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . PyTags.tags . getTerm

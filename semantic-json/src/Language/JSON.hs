-- | Semantic functionality for JSON programs.
module Language.JSON
( Term(..)
, TreeSitter.JSON.tree_sitter_json
) where

import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.JSON (tree_sitter_json)
import qualified TreeSitter.JSON.AST as JSON
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: JSON.Document a }

instance TS.Unmarshal Term where
  unmarshalNode node = Term <$> TS.unmarshalNode node

-- | Tags aren’t really meaningful for JSON, but by implementing this we can avoid having to customize the set of parsers used for computing tags.
instance Tags.ToTags Term where
  tags _ _ = []

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Semantic functionality for JSON programs.
module Language.JSON
( Term(..)
, TreeSitter.JSON.tree_sitter_json
) where

import           AST.Marshal.JSON
import           Data.Proxy
import qualified Language.JSON.AST as JSON
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.JSON (tree_sitter_json)
import qualified AST.Unmarshal as TS

newtype Term a = Term { getTerm :: JSON.Document a }
  deriving MarshalJSON

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy JSON.Document)
  showFailure _ = TS.showFailure (Proxy :: Proxy JSON.Document)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

-- | Tags aren’t really meaningful for JSON, but by implementing this we can avoid having to customize the set of parsers used for computing tags.
instance Tags.ToTags Term where
  tags _ _ = []

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Semantic functionality for JSON programs.
module Language.PHP
( Term(..)
, TreeSitter.PHP.tree_sitter_php
) where

import qualified AST.Unmarshal as TS
import           Data.Aeson (ToJSON (..), ToJSON1, toJSON1)
import           Data.Proxy
import qualified Language.PHP.AST as PHP
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.PHP (tree_sitter_php)

newtype Term a = Term { getTerm :: PHP.Program a }
  deriving newtype ToJSON1

instance ToJSON a => ToJSON (Term a) where
  toJSON = toJSON1

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy PHP.Program)
  showFailure _ = TS.showFailure (Proxy :: Proxy PHP.Program)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

-- | Tags aren’t really meaningful for JSON, but by implementing this we can avoid having to customize the set of parsers used for computing tags.
instance Tags.ToTags Term where
  tags _ _ = []

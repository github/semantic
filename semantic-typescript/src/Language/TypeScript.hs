{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
-- | Semantic functionality for TypeScript programs.
module Language.TypeScript
( Term(..)
, Language.TypeScript.Grammar.tree_sitter_typescript
) where

import qualified AST.Unmarshal as TS
import           Data.Aeson (ToJSON (..), ToJSON1, toJSON1)
import           Data.Proxy
import qualified Language.TypeScript.AST as TypeScript
import qualified Language.TypeScript.Grammar (tree_sitter_typescript)
import qualified Language.TypeScript.Tags as TsTags
import qualified Tags.Tagging.Precise as Tags

newtype Term a = Term { getTerm :: TypeScript.Program a }
  deriving newtype ToJSON1

instance ToJSON a => ToJSON (Term a) where
  toJSON = toJSON1

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy TypeScript.Program)
  showFailure _ = TS.showFailure (Proxy :: Proxy TypeScript.Program)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . TsTags.tags . getTerm

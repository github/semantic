{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
-- | Semantic functionality for TSX programs.
module Language.TSX
( Term(..)
, Language.TSX.Grammar.tree_sitter_tsx
) where

import           AST.Marshal.JSON
import           Data.Proxy
import qualified Language.TSX.AST as TSX
import qualified Language.TSX.Tags as TsxTags
import qualified Tags.Tagging.Precise as Tags
import qualified Language.TSX.Grammar (tree_sitter_tsx)
import qualified AST.Unmarshal as TS

newtype Term a = Term { getTerm :: TSX.Program a }
  deriving MarshalJSON

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy TSX.Program)
  showFailure _ = TS.showFailure (Proxy :: Proxy TSX.Program)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . TsxTags.tags . getTerm

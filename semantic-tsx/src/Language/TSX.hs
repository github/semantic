{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

-- | Semantic functionality for TSX programs.
module Language.TSX
  ( Term (..),
    Language.TSX.Grammar.tree_sitter_tsx,
  )
where

import qualified AST.Unmarshal as TS
import Data.Proxy
import qualified Language.TSX.AST as TSX
import qualified Language.TSX.Grammar (tree_sitter_tsx)
import qualified Language.TSX.Tags as TsxTags
import Scope.Graph.Convert
import qualified Tags.Tagging.Precise as Tags

newtype Term a = Term {getTerm :: TSX.Program a}

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy TSX.Program)
  showFailure _ = TS.showFailure (Proxy :: Proxy TSX.Program)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . TsxTags.tags . getTerm

instance ToScopeGraph Term where
  type FocalPoint Term _ = ()
  scopeGraph _ = todo "Implement scope graph for TSX"

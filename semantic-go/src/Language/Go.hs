{-# LANGUAGE TypeFamilies #-}

-- | Semantic functionality for Go programs.
module Language.Go
  ( Term (..),
    Language.Go.Grammar.tree_sitter_go,
  )
where

import qualified AST.Unmarshal as TS
import Data.Proxy
import qualified Language.Go.AST as Go
import qualified Language.Go.Grammar (tree_sitter_go)
import qualified Language.Go.Tags as GoTags
import Scope.Graph.Convert
import qualified Tags.Tagging.Precise as Tags

newtype Term a = Term {getTerm :: Go.SourceFile a}

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy Go.SourceFile)
  showFailure _ = TS.showFailure (Proxy :: Proxy Go.SourceFile)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . GoTags.tags . getTerm

instance ToScopeGraph Term where
  type FocalPoint Term _ = ()
  scopeGraph _ = todo "Implement scope graphs for Go"

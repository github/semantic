{-# LANGUAGE TypeFamilies #-}

-- | Semantic functionality for Java programs.
module Language.Java
  ( Term (..),
    Language.Java.Grammar.tree_sitter_java,
  )
where

import qualified AST.Unmarshal as TS
import Data.Proxy
import qualified Language.Java.AST as Java
import qualified Language.Java.Grammar (tree_sitter_java)
import qualified Language.Java.Tags as JavaTags
import Scope.Graph.Convert
import qualified Tags.Tagging.Precise as Tags

newtype Term a = Term {getTerm :: Java.Program a}

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy Java.Program)
  showFailure _ = TS.showFailure (Proxy :: Proxy Java.Program)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . JavaTags.tags . getTerm

instance ToScopeGraph Term where
  type FocalPoint Term = ()
  scopeGraph _ = todo "Implement scope graphs for Java"

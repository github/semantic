{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Semantic functionality for Go programs.
module Language.Go
( Term(..)
, Language.Go.Grammar.tree_sitter_go
) where

import           AST.Marshal.JSON
import           Data.Proxy
import qualified Language.Go.AST as Go
import qualified Language.Go.Tags as GoTags
import qualified Tags.Tagging.Precise as Tags
import qualified Language.Go.Grammar (tree_sitter_go)
import qualified AST.Unmarshal as TS

newtype Term a = Term { getTerm :: Go.SourceFile a }
  deriving MarshalJSON

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy Go.SourceFile)
  showFailure _ = TS.showFailure (Proxy :: Proxy Go.SourceFile)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . GoTags.tags . getTerm

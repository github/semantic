{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
-- | Semantic functionality for C programs.
module Language.C
( Term(..)
, TreeSitter.C.tree_sitter_c
) where

import           AST.Marshal.JSON
import qualified AST.Unmarshal as TS
import           Data.Proxy
import qualified Language.C.AST as C
import qualified Language.C.Tags as CTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.C (tree_sitter_c)

newtype Term a = Term { getTerm :: C.TranslationUnit a }
  deriving MarshalJSON

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy C.TranslationUnit)
  showFailure _ = TS.showFailure (Proxy :: Proxy C.TranslationUnit)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . CTags.tags . getTerm

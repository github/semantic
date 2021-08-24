{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Semantic functionality for OCaml programs.
module Language.OCaml
( Term(..)
, Language.OCaml.Grammar.tree_sitter_ocaml
) where

import           AST.Marshal.JSON
import qualified AST.Unmarshal as TS
import           Data.Proxy
import qualified Language.OCaml.AST as OCaml
import qualified Language.OCaml.Grammar (tree_sitter_ocaml)
import qualified Language.OCaml.Tags as OCamlTags
import qualified Tags.Tagging.Precise as Tags

newtype Term a = Term { getTerm :: OCaml.CompilationUnit a }
  deriving MarshalJSON

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy OCaml.CompilationUnit)
  showFailure _ = TS.showFailure (Proxy :: Proxy OCaml.CompilationUnit)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . OCamlTags.tags . getTerm

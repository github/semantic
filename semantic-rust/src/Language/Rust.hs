-- | Semantic functionality for Rust programs.
module Language.Rust
( Term(..)
, Language.Rust.Grammar.tree_sitter_rust
) where

import           Data.Proxy
import qualified Language.Rust.AST as Rust
import qualified Language.Rust.Tags as RustTags
import qualified Tags.Tagging.Precise as Tags
import qualified Language.Rust.Grammar (tree_sitter_rust)
import qualified AST.Unmarshal as TS

newtype Term a = Term { getTerm :: Rust.SourceFile a }

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy Rust.SourceFile)
  showFailure _ = TS.showFailure (Proxy :: Proxy Rust.SourceFile)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . RustTags.tags . getTerm

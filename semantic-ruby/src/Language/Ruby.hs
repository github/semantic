{-# LANGUAGE TypeApplications #-}

-- | Semantic functionality for Ruby programs.
module Language.Ruby
( Term(..)
, TreeSitter.Ruby.tree_sitter_ruby
) where

import           Control.Carrier.State.Strict
import           Data.Proxy
import           Data.Text (Text)
import qualified Language.Ruby.AST as Rb
import qualified Language.Ruby.Tags as RbTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Ruby (tree_sitter_ruby)
import qualified AST.Unmarshal as TS

newtype Term a = Term { getTerm :: Rb.Program a }

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy Rb.Program)
  showFailure _ = TS.showFailure (Proxy :: Proxy Rb.Program)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . evalState @[Text] [] . RbTags.tags . getTerm

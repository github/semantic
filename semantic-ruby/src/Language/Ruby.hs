{-# LANGUAGE TypeApplications #-}

-- | Semantic functionality for Ruby programs.
module Language.Ruby
( Term(..)
, TreeSitter.Ruby.tree_sitter_ruby
) where


import Control.Carrier.State.Strict
import Data.Text (Text)
import qualified Language.Ruby.Tags as PyTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Ruby (tree_sitter_ruby)
import qualified TreeSitter.Ruby.AST as Rb
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: Rb.Program a }

instance TS.SymbolMatching Term where
  showFailure _ _ = "failed for Term"

instance TS.Unmarshal Term where
  matchers = fmap (TS.hoist Term) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . evalState @[Text] [] . PyTags.tags . getTerm

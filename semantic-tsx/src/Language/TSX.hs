{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
-- | Semantic functionality for TSX programs.
module Language.TSX
( Term(..)
, TreeSitter.TSX.tree_sitter_tsx
) where

import           Data.Proxy
import qualified Language.TSX.Tags as TsxTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.TSX (tree_sitter_tsx)
import qualified TreeSitter.TSX.AST as TSX
import qualified TreeSitter.Unmarshal as TS

import qualified Data.Term as Term
import Diffing.Interpreter

newtype Term a = Term { getTerm :: TSX.Program a }

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy TSX.Program)
  showFailure _ = TS.showFailure (Proxy :: Proxy TSX.Program)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . TsxTags.tags . getTerm

instance DiffTerms Term
  -- diffTermPair :: Edit (term ann1) (term ann2) -> Diff.Diff (Syntax term) ann1 ann2

instance Term.IsTerm Term where
  type Syntax Term = Term
  -- toTermF :: term ann -> TermF (Syntax term) ann (term ann)
  -- toTermF = coerce
  -- fromTermF :: TermF (Syntax term) ann (term ann) -> term ann
  -- fromTermF = coerce

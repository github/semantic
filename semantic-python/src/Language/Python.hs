{-# LANGUAGE TypeApplications #-}
-- | Semantic functionality for Python programs.
module Language.Python
( Term(..)
, Language.Python.Grammar.tree_sitter_python
, graphPythonFile
) where

import           Analysis.Name (Name)
import qualified AST.Unmarshal as TS
import           Control.Carrier.Sketch.Fresh
import qualified Data.ByteString as ByteString
import           Data.Proxy
import           Data.ScopeGraph (ScopeGraph)
import qualified Language.Python.AST as Py
import           Language.Python.Grammar (tree_sitter_python)
import           Language.Python.ScopeGraph
import qualified Language.Python.Tags as PyTags
import           ScopeGraph.Convert
import           Source.Loc (Loc)
import           System.Exit (die)
import qualified System.Path as Path
import qualified Tags.Tagging.Precise as Tags

newtype Term a = Term { getTerm :: Py.Module a }

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy Py.Module)
  showFailure _ = TS.showFailure (Proxy :: Proxy Py.Module)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . PyTags.tags . getTerm

instance ToScopeGraph Term where
  scopeGraph = scopeGraphModule . getTerm

graphPythonFile :: Path.AbsRelFile -> IO (ScopeGraph Name, Result)
graphPythonFile fp = do
  file <- ByteString.readFile (Path.toString fp)
  tree <- TS.parseByteString @Term @Loc tree_sitter_python file
  pyModule <- either die pure tree
  runSketch (Just fp) $ scopeGraph pyModule

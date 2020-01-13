{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Main (main) where

import           Control.Algebra
import           Control.Carrier.Sketch.Fresh
import           Control.Monad
import           Convert.ToScopeGraph
import qualified Data.ByteString as ByteString
import qualified Data.ScopeGraph as ScopeGraph
import qualified Language.Python ()
import           Source.Loc
import qualified Source.Source as Source
import           System.Exit (die)
import qualified System.Path as Path
import qualified TreeSitter.Python as TSP
import qualified TreeSitter.Python.AST as Py
import qualified TreeSitter.Unmarshal as TS

{-

The Python code here is

hello = ()
goodbye = ()

The graph should be

  🏁
  |
  1️⃣----"hello"
  |
  |
  |
  |
  2️⃣----"goodbye"

-}


runScopeGraph :: ScopeGraph.ToScopeGraph t => Path.AbsRelFile -> Source.Source -> t Loc -> ScopeGraph.ScopeGraph ScopeGraph.Info
runScopeGraph p _src item = fst . run . runSketch (Just p) $ ScopeGraph.scopeGraph item

sampleGraphThing :: (Has (Sketch ScopeGraph.Info) sig m) => m ()
sampleGraphThing = do
  declare @ScopeGraph.Info "hello" DeclProperties
  declare @ScopeGraph.Info "goodbye" DeclProperties


assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a b = unless (a == b) (die (show a <> "\ndoes not equal\n" <> show b))

main :: IO ()
main = do
  let path = "semantic-python/test/fixtures/1-01-empty-module.py"
  file <- ByteString.readFile path
  tree <- TS.parseByteString @Py.Module @Loc TSP.tree_sitter_python file
  pyModule <- either die pure tree
  let expecto = fst . run $ runSketch Nothing sampleGraphThing
  let result = runScopeGraph (Path.absRel path) (Source.fromUTF8 file) pyModule
  print result
  assertEqual expecto result


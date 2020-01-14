{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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


runScopeGraph :: ToScopeGraph t => Path.AbsRelFile -> Source.Source -> t Loc -> (ScopeGraph.ScopeGraph Addr, Result)
runScopeGraph p _src item = run . runSketch @Addr (Just p) $ scopeGraph item

sampleGraphThing :: (Has (Sketch Addr) sig m) => m Result
sampleGraphThing = do
  declare @Addr "hello" DeclProperties
  declare @Addr "goodbye" DeclProperties
  pure Complete


assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a b = unless (a == b) (die (show a <> "\ndoes not equal\n" <> show b))

main :: IO ()
main = do
  let path = "semantic-python/test/fixtures/1-04-toplevel-assignment.py"
  file <- ByteString.readFile path
  tree <- TS.parseByteString @Py.Module @Loc TSP.tree_sitter_python file
  pyModule <- either die pure tree
  let (expecto, Complete) = run $ runSketch @Addr Nothing sampleGraphThing
  let (result, Complete) = runScopeGraph (Path.absRel path) (Source.fromUTF8 file) pyModule
  print result
  assertEqual expecto result


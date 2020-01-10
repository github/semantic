{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Main (main) where

import           Control.Monad
import qualified Data.ByteString as ByteString
import qualified Data.ScopeGraph as ScopeGraph
import qualified Language.Python ()
import           Source.Loc
import qualified Source.Source as Source
import           System.Exit (die)
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

needed :: IO (ScopeGraph.ScopeGraph ScopeGraph.Info)
needed = do
  let start = ScopeGraph.root
  one   <- ScopeGraph.scope
  hello <- ScopeGraph.ref "hello"
  two   <- ScopeGraph.scope
  goodb <- ScopeGraph.ref "goodbye"
  pure . ScopeGraph.edges $ [ (start, one)
                            , (one, hello)
                            , (one, two)
                            , (two, goodb)
                            ]

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a b = unless (a == b) (die (show a <> "\ndoes not equal\n" <> show b))

main :: IO ()
main = do
  file <- ByteString.readFile "semantic-python/test/fixtures/1-01-empty-module.py"
  tree <- TS.parseByteString @Py.Module @Loc TSP.tree_sitter_python file
  pyModule <- either die pure tree
  expecto <- needed
  result <- ScopeGraph.runScopeGraph (Source.fromUTF8 file) pyModule
  print result
  assertEqual expecto result


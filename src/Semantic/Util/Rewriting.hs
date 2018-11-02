{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-export-lists -Wno-incomplete-uni-patterns #-}
module Semantic.Util.Rewriting where

import Prelude hiding (id, readFile, (.))
import Prologue

import           Control.Category
import qualified Data.ByteString.Char8 as BC
import           Text.Show.Pretty (pPrint)

import           Control.Matching
import           Control.Rewriting hiding (fromMatcher, target)
import           Data.Blob
import           Data.File
import           Data.History
import qualified Data.Language as Language
import qualified Data.Source as Source
import qualified Data.Syntax.Literal as Literal
import           Data.Term
import           Language.JSON.PrettyPrint
import           Language.Python.PrettyPrint
import           Language.Ruby.PrettyPrint
import           Parsing.Parser
import           Reprinting.Pipeline
import           Semantic.Task

import Tags.Tagging
import Tags.Taggable
import Data.Machine
import Data.Machine.Source

testPythonFile = do
  let path = "test/fixtures/python/reprinting/function.py"
  src  <- blobSource <$> readBlobFromFile' (File path Language.Python)
  tree <- parseFile' miniPythonParser path
  pure (src, tree)

testPythonPipeline = do
  (src, tree) <- testPythonFile
  printToTerm $ runReprinter src printingPython (mark Refactored tree)

testPythonPipeline' = do
  (src, tree) <- testPythonFile
  pure $ runTokenizing src (mark Refactored tree)

testPythonPipeline'' = do
  (src, tree) <- testPythonFile
  pure $ runContextualizing src (mark Refactored tree)

testPythonPipeline''' = do
  (src, tree) <- testPythonFile
  pure $ runTranslating src printingPython (mark Refactored tree)


testPythonDefs path = do
  blob <- readBlobFromFile' (File path Language.Python)
  tree <- parseFile' pythonParser path
  -- pure . Data.Machine.run $ Data.Machine.Source.source (tagging blob tree)
  pure $! runTagging blob tree

testGoDefs path = do
  blob <- readBlobFromFile' (File path Language.Go)
  tree <- parseFile' goParser path
  -- pure . Data.Machine.run $ Data.Machine.Source.source (tagging blob tree)
  pure $! runTagging blob tree

testRubyDefs path = do
  blob <- readBlobFromFile' (File path Language.Ruby)
  tree <- parseFile' rubyParser path
  -- pure . Data.Machine.run $ Data.Machine.Source.source (tagging blob tree)
  pure $! runTagging blob tree


testRubyFile = do
  let path = "test/fixtures/ruby/reprinting/infix.rb"
  src  <- blobSource <$> readBlobFromFile' (File path Language.Ruby)
  tree <- parseFile' miniRubyParser path
  pure (src, tree)

testRubyPipeline = do
  (src, tree) <- testRubyFile
  printToTerm $ runReprinter src printingRuby (mark Refactored tree)

testRubyPipeline' = do
  (src, tree) <- testRubyFile
  pure $ runTokenizing src (mark Refactored tree)

testRubyPipeline'' = do
  (src, tree) <- testRubyFile
  pure $ runContextualizing src (mark Refactored tree)

testJSONPipeline = do
  (src, tree) <- testJSONFile
  printToTerm $ runReprinter src defaultJSONPipeline (mark Refactored tree)

printToTerm = either (putStrLn . show) (BC.putStr . Source.sourceBytes)

testJSONFile = do
  let path = "test/fixtures/javascript/reprinting/map.json"
  src  <- blobSource <$> readBlobFromFile' (File path Language.JSON)
  tree <- parseFile' jsonParser path
  pure (src, tree)

renameKey :: ( Literal.TextElement :< fs
             , Apply Functor fs
             , term ~ Term (Sum fs) History
             )
          => Rewrite (env, term) (Literal.KeyValue term)
renameKey = do
  Literal.KeyValue k v <- id
  guard (projectTerm k == Just (Literal.TextElement "\"foo\""))
  new <- modified (Literal.TextElement "\"fooA\"")
  pure (Literal.KeyValue new v)

testRenameKey = do
  (src, tree) <- testJSONFile
  let (Right tagged) = rewrite (somewhere' renameKey) () (mark Unmodified tree)
  pPrint tagged
  printToTerm $ runReprinter src defaultJSONPipeline tagged

increaseNumbers :: (term ~ Term (Sum fs) History) => Rewrite (env, term) (Literal.Float term)
increaseNumbers = do
  (Literal.Float c) <- id
  pure (Literal.Float (c <> "0"))

addKVPair :: ( Literal.TextElement :< syn
             , Literal.KeyValue :< syn
             , Literal.Array :< syn
             , Apply Functor syn
             , term ~ Term (Sum syn) History
             ) => Rewrite (env, term) (Literal.Hash term)
addKVPair = do
  Literal.Hash els <- id
  k <- modified $ Literal.TextElement "\"added\""
  v <- modified $ Literal.Array []
  pair <- modified $ Literal.KeyValue k v
  pure (Literal.Hash (pair : els))

testAddKVPair = do
  (src, tree) <- testJSONFile
  let (Right tagged) = rewrite (somewhere addKVPair markRefactored) () (mark Unmodified tree)
  printToTerm $ runReprinter src defaultJSONPipeline tagged

overwriteFloats :: Rewrite (env, term) (Literal.Float term)
overwriteFloats = pure (Literal.Float "0")

testOverwriteFloats = do
  (src, tree) <- testJSONFile
  let (Right tagged) = rewrite (somewhere overwriteFloats markRefactored) () (mark Unmodified tree)
  pPrint tagged
  printToTerm $ runReprinter src defaultJSONPipeline tagged

kvMatcher :: forall fs term .
  ( Literal.KeyValue :< fs
  , Literal.TextElement :< fs
  , term ~ Term (Sum fs) History
  ) =>
  Text -> Matcher term (Literal.KeyValue term)
kvMatcher name = matchM projectTerm target <* matchKey where
  matchKey
    = match Literal.key .
        match Literal.textElementContent $
          ensure (== name)

changeKV :: ( Apply Functor syntax
            , Literal.Array :< syntax
            , Literal.Float :< syntax
            , term ~ Term (Sum syntax) History
            )
         => Rewrite (env, term) (Literal.KeyValue term)
changeKV = do
  (Literal.KeyValue k v) <- id
  (Literal.Array vals) <- guardTerm v
  let float = remark Refactored (injectTerm (annotation v) (Literal.Float "4"))
  let newArr = remark Refactored (injectTerm (annotation v) (Literal.Array (float:vals)))
  pure (Literal.KeyValue k newArr)

testChangeKV = do
  (src, tree) <- testJSONFile
  let (Right tagged) = rewrite (somewhere' changeKV) () (mark Unmodified tree)
  printToTerm $ runReprinter src defaultJSONPipeline tagged

parseFile' :: Parser term -> FilePath -> IO term
parseFile' parser = runTask . (parse parser <=< readBlob . file)

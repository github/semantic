module Semantic.CLI.Spec (spec) where

import           Control.Monad (when)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Foldable (for_)
import           Semantic.CLI
import           Semantic.IO
import           Semantic.Task

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "runDiff" $
    for_ diffFixtures $ \ (diffRenderer, runDiff, files, expected) ->
      it ("renders to " <> diffRenderer <> " with files " <> show files) $ do
        output <- runTask $ readBlobPairs (Right files) >>= runDiff
        runBuilder output `shouldBe'` expected

  describe "runParse" $
    for_ parseFixtures $ \ (parseTreeRenderer, runParse, files, expected) ->
      it ("renders to " <> parseTreeRenderer <> " with files " <> show files) $ do
        output <- runTask $ readBlobs (Right files) >>= runParse
        runBuilder output `shouldBe'` expected
  where
    shouldBe' actual' expectedFile = do
      let actual = verbatim actual'
      expected <- verbatim <$> B.readFile expectedFile
      actual `shouldBe` expected

parseFixtures :: [(String, [Blob] -> TaskEff Builder, [File], FilePath)]
parseFixtures =
  [ (show SExpressionTermRenderer, runParse SExpressionTermRenderer, path, "test/fixtures/ruby/corpus/and-or.parseA.txt")
  , (show JSONTermRenderer, runParse JSONTermRenderer, path, prefix </> "parse-tree.json")
  , (show JSONTermRenderer, runParse JSONTermRenderer, path', prefix </> "parse-trees.json")
  , (show JSONTermRenderer, runParse JSONTermRenderer, [], prefix </> "parse-tree-empty.json")
  , (show (SymbolsTermRenderer defaultSymbolFields), runParse (SymbolsTermRenderer defaultSymbolFields), path'', prefix </> "parse-tree.symbols.json")
  ]
  where path = [File "test/fixtures/ruby/corpus/and-or.A.rb" Ruby]
        path' = [File "test/fixtures/ruby/corpus/and-or.A.rb" Ruby, File "test/fixtures/ruby/corpus/and-or.B.rb" Ruby]
        path'' = [File "test/fixtures/ruby/corpus/method-declaration.A.rb" Ruby]
        prefix = "test/fixtures/cli"

diffFixtures :: [(String, [BlobPair] -> TaskEff Builder, [Both File], FilePath)]
diffFixtures =
  [ (show JSONDiffRenderer, runDiff JSONDiffRenderer, pathMode, prefix </> "diff-tree.json")
  , (show SExpressionDiffRenderer, runDiff SExpressionDiffRenderer, pathMode, "test/fixtures/ruby/corpus/method-declaration.diffA-B.txt")
  , (show ToCDiffRenderer, runDiff ToCDiffRenderer, pathMode, prefix </> "diff-tree.toc.json")
  ]
  where pathMode = [both (File "test/fixtures/ruby/corpus/method-declaration.A.rb" Ruby) (File "test/fixtures/ruby/corpus/method-declaration.B.rb"  Ruby)]
        prefix = "test/fixtures/cli"

module Semantic.CLI.Spec (spec) where

import Control.Monad (when)
import Data.ByteString.Builder
import Data.Foldable (for_)
import Semantic.CLI
import Semantic.IO
import Semantic.Task

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
    shouldBe' actual expected = do
      when (actual /= expected) $ print actual
      actual `shouldBe` expected

parseFixtures :: [(String, [Blob] -> TaskEff Builder, [File], ByteString)]
parseFixtures =
  [ (show SExpressionTermRenderer, runParse SExpressionTermRenderer, pathMode, sExpressionParseTreeOutput)
  , (show JSONTermRenderer, runParse JSONTermRenderer, pathMode, jsonParseTreeOutput)
  , (show JSONTermRenderer, runParse JSONTermRenderer, pathMode', jsonParseTreeOutput')
  , (show JSONTermRenderer, runParse JSONTermRenderer, [], emptyJsonParseTreeOutput)
  , (show (SymbolsTermRenderer defaultSymbolFields), runParse (SymbolsTermRenderer defaultSymbolFields), [File "test/fixtures/ruby/corpus/method-declaration.A.rb" (Just Ruby)], symbolsOutput)
  , (show TagsTermRenderer, runParse TagsTermRenderer, [File "test/fixtures/ruby/corpus/method-declaration.A.rb" (Just Ruby)], tagsOutput)
  ]
  where pathMode = [File "test/fixtures/ruby/corpus/and-or.A.rb" (Just Ruby)]
        pathMode' = [File "test/fixtures/ruby/corpus/and-or.A.rb" (Just Ruby), File "test/fixtures/ruby/corpus/and-or.B.rb" (Just Ruby)]

        sExpressionParseTreeOutput = "(Program\n  (LowAnd\n    (Send\n      (Identifier))\n    (Send\n      (Identifier))))\n"
        jsonParseTreeOutput = "{\"trees\":[{\"programNode\":{\"category\":\"Program\",\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]},\"children\":[{\"category\":\"LowAnd\",\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]},\"children\":[{\"category\":\"Send\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]},\"children\":[{\"name\":\"foo\",\"category\":\"Identifier\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]},\"children\":[]}]},{\"category\":\"Send\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]},\"children\":[{\"name\":\"bar\",\"category\":\"Identifier\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]},\"children\":[]}]}]}]},\"path\":\"test/fixtures/ruby/corpus/and-or.A.rb\",\"language\":\"Ruby\"}]}\n"
        jsonParseTreeOutput' = "{\"trees\":[{\"programNode\":{\"category\":\"Program\",\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]},\"children\":[{\"category\":\"LowAnd\",\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]},\"children\":[{\"category\":\"Send\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]},\"children\":[{\"name\":\"foo\",\"category\":\"Identifier\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]},\"children\":[]}]},{\"category\":\"Send\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]},\"children\":[{\"name\":\"bar\",\"category\":\"Identifier\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]},\"children\":[]}]}]}]},\"path\":\"test/fixtures/ruby/corpus/and-or.A.rb\",\"language\":\"Ruby\"},{\"programNode\":{\"category\":\"Program\",\"sourceRange\":[0,24],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]},\"children\":[{\"category\":\"LowOr\",\"sourceRange\":[0,10],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,11]},\"children\":[{\"category\":\"Send\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]},\"children\":[{\"name\":\"foo\",\"category\":\"Identifier\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]},\"children\":[]}]},{\"category\":\"Send\",\"sourceRange\":[7,10],\"sourceSpan\":{\"start\":[1,8],\"end\":[1,11]},\"children\":[{\"name\":\"bar\",\"category\":\"Identifier\",\"sourceRange\":[7,10],\"sourceSpan\":{\"start\":[1,8],\"end\":[1,11]},\"children\":[]}]}]},{\"category\":\"LowAnd\",\"sourceRange\":[11,23],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,13]},\"children\":[{\"category\":\"LowOr\",\"sourceRange\":[11,17],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,7]},\"children\":[{\"category\":\"Send\",\"sourceRange\":[11,12],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,2]},\"children\":[{\"name\":\"a\",\"category\":\"Identifier\",\"sourceRange\":[11,12],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,2]},\"children\":[]}]},{\"category\":\"Send\",\"sourceRange\":[16,17],\"sourceSpan\":{\"start\":[2,6],\"end\":[2,7]},\"children\":[{\"name\":\"b\",\"category\":\"Identifier\",\"sourceRange\":[16,17],\"sourceSpan\":{\"start\":[2,6],\"end\":[2,7]},\"children\":[]}]}]},{\"category\":\"Send\",\"sourceRange\":[22,23],\"sourceSpan\":{\"start\":[2,12],\"end\":[2,13]},\"children\":[{\"name\":\"c\",\"category\":\"Identifier\",\"sourceRange\":[22,23],\"sourceSpan\":{\"start\":[2,12],\"end\":[2,13]},\"children\":[]}]}]}]},\"path\":\"test/fixtures/ruby/corpus/and-or.B.rb\",\"language\":\"Ruby\"}]}\n"
        emptyJsonParseTreeOutput = "{\"trees\":[]}\n"
        symbolsOutput = "{\"files\":[{\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb\",\"symbols\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"kind\":\"Method\",\"symbol\":\"foo\"}],\"language\":\"Ruby\"}]}\n"
        tagsOutput = "[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb\",\"kind\":\"Method\",\"symbol\":\"foo\",\"line\":\"def foo\",\"language\":\"Ruby\"}]\n"


diffFixtures :: [(String, [BlobPair] -> TaskEff Builder, [Both File], ByteString)]
diffFixtures =
  [ (show JSONDiffRenderer, runDiff JSONDiffRenderer, pathMode, jsonOutput)
  , (show SExpressionDiffRenderer, runDiff SExpressionDiffRenderer, pathMode, sExpressionOutput)
  , (show ToCDiffRenderer, runDiff ToCDiffRenderer, pathMode, tocOutput)
  ]
  where pathMode = [both (File "test/fixtures/ruby/corpus/method-declaration.A.rb" (Just Ruby)) (File "test/fixtures/ruby/corpus/method-declaration.B.rb"  (Just Ruby))]

        jsonOutput =  "{\"diffs\":[{\"diff\":{\"merge\":{\"before\":{\"category\":\"Program\",\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},\"after\":{\"category\":\"Program\",\"sourceRange\":[0,21],\"sourceSpan\":{\"start\":[1,1],\"end\":[4,1]}},\"children\":[{\"merge\":{\"before\":{\"category\":\"Method\",\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}},\"after\":{\"category\":\"Method\",\"sourceRange\":[0,20],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,4]}},\"children\":[{\"merge\":{\"before\":{\"category\":\"Empty\",\"sourceRange\":[0,0],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,1]}},\"after\":{\"category\":\"Empty\",\"sourceRange\":[0,0],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,1]}},\"children\":[]}},{\"patch\":{\"replace\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"foo\",\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"category\":\"Identifier\",\"children\":[],\"name\":\"bar\",\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}}]}},{\"patch\":{\"insert\":{\"category\":\"Identifier\",\"children\":[],\"name\":\"a\",\"sourceRange\":[8,9],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,10]}}}},{\"merge\":{\"before\":{\"category\":\"[]\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,4]}},\"after\":{\"category\":\"\",\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}},\"children\":[{\"patch\":{\"insert\":{\"category\":\"Send\",\"children\":[{\"patch\":{\"insert\":{\"category\":\"Identifier\",\"children\":[],\"name\":\"baz\",\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}}}}],\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}}}}]}}]}}]}},\"stat\":{\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb -> test/fixtures/ruby/corpus/method-declaration.B.rb\",\"replace\":[{\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb\",\"language\":\"Ruby\"},{\"path\":\"test/fixtures/ruby/corpus/method-declaration.B.rb\",\"language\":\"Ruby\"}]}}]}\n"
        sExpressionOutput = "(Program\n  (Method\n    (Empty)\n  { (Identifier)\n  ->(Identifier) }\n  {+(Identifier)+}\n    (\n    {+(Send\n      {+(Identifier)+})+})))\n"
        tocOutput = "{\"changes\":{\"test/fixtures/ruby/corpus/method-declaration.A.rb -> test/fixtures/ruby/corpus/method-declaration.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"}]},\"errors\":{}}\n"

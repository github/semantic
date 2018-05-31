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
        jsonParseTreeOutput = "{\"trees\":[{\"tree\":{\"term\":\"Program\",\"children\":[{\"children\":{\"term\":\"Send\",\"sourceRange\":[8,11],\"sendReceiver\":null,\"sendBlock\":null,\"sendArgs\":[],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]},\"sendSelector\":{\"term\":\"Identifier\",\"name\":\"bar\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}},\"term\":\"LowAnd\",\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}},\"path\":\"test/fixtures/ruby/corpus/and-or.A.rb\",\"language\":\"Ruby\"}]}\n"
        jsonParseTreeOutput' = "{\"trees\":[{\"tree\":{\"term\":\"Program\",\"children\":[{\"children\":{\"term\":\"Send\",\"sourceRange\":[8,11],\"sendReceiver\":null,\"sendBlock\":null,\"sendArgs\":[],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]},\"sendSelector\":{\"term\":\"Identifier\",\"name\":\"bar\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}},\"term\":\"LowAnd\",\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}},\"path\":\"test/fixtures/ruby/corpus/and-or.A.rb\",\"language\":\"Ruby\"},{\"tree\":{\"term\":\"Program\",\"children\":[{\"children\":{\"term\":\"Send\",\"sourceRange\":[7,10],\"sendReceiver\":null,\"sendBlock\":null,\"sendArgs\":[],\"sourceSpan\":{\"start\":[1,8],\"end\":[1,11]},\"sendSelector\":{\"term\":\"Identifier\",\"name\":\"bar\",\"sourceRange\":[7,10],\"sourceSpan\":{\"start\":[1,8],\"end\":[1,11]}}},\"term\":\"LowOr\",\"sourceRange\":[0,10],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,11]}},{\"children\":{\"term\":\"Send\",\"sourceRange\":[22,23],\"sendReceiver\":null,\"sendBlock\":null,\"sendArgs\":[],\"sourceSpan\":{\"start\":[2,12],\"end\":[2,13]},\"sendSelector\":{\"term\":\"Identifier\",\"name\":\"c\",\"sourceRange\":[22,23],\"sourceSpan\":{\"start\":[2,12],\"end\":[2,13]}}},\"term\":\"LowAnd\",\"sourceRange\":[11,23],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,13]}}],\"sourceRange\":[0,24],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},\"path\":\"test/fixtures/ruby/corpus/and-or.B.rb\",\"language\":\"Ruby\"}]}\n"
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

        jsonOutput = "{\"diffs\":[{\"diff\":{\"merge\":{\"term\":\"Program\",\"children\":[{\"merge\":{\"after\":{\"sourceRange\":[0,20],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,4]}},\"methodParameters\":[{\"patch\":{\"insert\":{\"term\":\"Identifier\",\"name\":\"a\",\"sourceRange\":[8,9],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,10]}}}}],\"methodName\":{\"patch\":{\"replace\":[{\"term\":\"Identifier\",\"name\":\"foo\",\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"term\":\"Identifier\",\"name\":\"bar\",\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}}]}},\"methodContext\":[],\"methodReceiver\":{\"merge\":{\"after\":{\"sourceRange\":[0,0],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,1]}},\"term\":\"Empty\",\"before\":{\"sourceRange\":[0,0],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,1]}}}},\"term\":\"Method\",\"methodBody\":{\"merge\":{\"after\":{\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}},\"children\":[{\"patch\":{\"insert\":{\"term\":\"Send\",\"sourceRange\":[13,16],\"sendReceiver\":null,\"sendBlock\":null,\"sendArgs\":[],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]},\"sendSelector\":{\"patch\":{\"insert\":{\"term\":\"Identifier\",\"name\":\"baz\",\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}}}}}}}],\"before\":{\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,4]}}}},\"before\":{\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}}}}],\"before\":{\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},\"after\":{\"sourceRange\":[0,21],\"sourceSpan\":{\"start\":[1,1],\"end\":[4,1]}}}},\"stat\":{\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb -> test/fixtures/ruby/corpus/method-declaration.B.rb\",\"replace\":[{\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb\",\"language\":\"Ruby\"},{\"path\":\"test/fixtures/ruby/corpus/method-declaration.B.rb\",\"language\":\"Ruby\"}]}}]}\n"
        sExpressionOutput = "(Program\n  (Method\n    (Empty)\n  { (Identifier)\n  ->(Identifier) }\n  {+(Identifier)+}\n    (Statements\n    {+(Send\n      {+(Identifier)+})+})))\n"
        tocOutput = "{\"changes\":{\"test/fixtures/ruby/corpus/method-declaration.A.rb -> test/fixtures/ruby/corpus/method-declaration.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"}]},\"errors\":{}}\n"

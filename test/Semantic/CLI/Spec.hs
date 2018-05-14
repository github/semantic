module Semantic.CLI.Spec (spec) where

import Control.Monad (when)
import Data.Foldable (for_)
import Semantic.CLI
import Semantic.IO

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "runDiff" $
    for_ diffFixtures $ \ (diffRenderer, diffMode, expected) ->
      it ("renders to " <> show diffRenderer <> " in mode " <> show diffMode) $ do
        output <- runTask $ runDiff diffRenderer diffMode
        runBuilder output `shouldBe'` expected

  describe "runParse" $
    for_ parseFixtures $ \ (parseTreeRenderer, parseMode, expected) ->
      it ("renders to " <> show parseTreeRenderer <> " in mode " <> show parseMode) $ do
        output <- runTask $ runParse parseTreeRenderer parseMode
        runBuilder output `shouldBe'` expected
  where
    shouldBe' actual expected = do
      when (actual /= expected) $ print actual
      actual `shouldBe` expected

parseFixtures :: [(SomeRenderer TermRenderer, Either (Handle 'ReadMode) [File], ByteString)]
parseFixtures =
  [ (SomeRenderer SExpressionTermRenderer, pathMode, sExpressionParseTreeOutput)
  , (SomeRenderer JSONTermRenderer, pathMode, jsonParseTreeOutput)
  , (SomeRenderer JSONTermRenderer, pathMode', jsonParseTreeOutput')
  , (SomeRenderer JSONTermRenderer, Right [], emptyJsonParseTreeOutput)
  , (SomeRenderer (SymbolsTermRenderer defaultSymbolFields), Right [File "test/fixtures/ruby/corpus/method-declaration.A.rb" (Just Ruby)], symbolsOutput)
  , (SomeRenderer TagsTermRenderer, Right [File "test/fixtures/ruby/corpus/method-declaration.A.rb" (Just Ruby)], tagsOutput)
  ]
  where pathMode = Right [File "test/fixtures/ruby/corpus/and-or.A.rb" (Just Ruby)]
        pathMode' = Right [File "test/fixtures/ruby/corpus/and-or.A.rb" (Just Ruby), File "test/fixtures/ruby/corpus/and-or.B.rb" (Just Ruby)]

        sExpressionParseTreeOutput = "(Program\n  (LowAnd\n    (Send\n      (Identifier))\n    (Send\n      (Identifier))))\n"
        jsonParseTreeOutput = "{\"trees\":[{\"path\":\"test/fixtures/ruby/corpus/and-or.A.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"LowAnd\",\"children\":[{\"category\":\"Send\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"foo\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}}],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"Send\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"bar\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}},\"language\":\"Ruby\"}]}\n"
        jsonParseTreeOutput' = "{\"trees\":[{\"path\":\"test/fixtures/ruby/corpus/and-or.A.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"LowAnd\",\"children\":[{\"category\":\"Send\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"foo\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}}],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"Send\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"bar\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}},\"language\":\"Ruby\"},{\"path\":\"test/fixtures/ruby/corpus/and-or.B.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"LowOr\",\"children\":[{\"category\":\"Send\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"foo\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}}],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"Send\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"bar\",\"sourceRange\":[7,10],\"sourceSpan\":{\"start\":[1,8],\"end\":[1,11]}}],\"sourceRange\":[7,10],\"sourceSpan\":{\"start\":[1,8],\"end\":[1,11]}}],\"sourceRange\":[0,10],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,11]}},{\"category\":\"LowAnd\",\"children\":[{\"category\":\"LowOr\",\"children\":[{\"category\":\"Send\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"a\",\"sourceRange\":[11,12],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,2]}}],\"sourceRange\":[11,12],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,2]}},{\"category\":\"Send\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"b\",\"sourceRange\":[16,17],\"sourceSpan\":{\"start\":[2,6],\"end\":[2,7]}}],\"sourceRange\":[16,17],\"sourceSpan\":{\"start\":[2,6],\"end\":[2,7]}}],\"sourceRange\":[11,17],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,7]}},{\"category\":\"Send\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"c\",\"sourceRange\":[22,23],\"sourceSpan\":{\"start\":[2,12],\"end\":[2,13]}}],\"sourceRange\":[22,23],\"sourceSpan\":{\"start\":[2,12],\"end\":[2,13]}}],\"sourceRange\":[11,23],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,13]}}],\"sourceRange\":[0,24],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},\"language\":\"Ruby\"}]}\n"
        emptyJsonParseTreeOutput = "{\"trees\":[]}\n"
        symbolsOutput = "{\"files\":[{\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb\",\"symbols\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"kind\":\"Method\",\"symbol\":\"foo\"}],\"language\":\"Ruby\"}]}\n"
        tagsOutput = "[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb\",\"kind\":\"Method\",\"symbol\":\"foo\",\"line\":\"def foo\",\"language\":\"Ruby\"}]\n"


diffFixtures :: [(SomeRenderer DiffRenderer, Either (Handle 'ReadMode) [Both File], ByteString)]
diffFixtures =
  [ (SomeRenderer JSONDiffRenderer, pathMode, jsonOutput)
  , (SomeRenderer SExpressionDiffRenderer, pathMode, sExpressionOutput)
  , (SomeRenderer ToCDiffRenderer, pathMode, tocOutput)
  ]
  where pathMode = Right [both (File "test/fixtures/ruby/corpus/method-declaration.A.rb" (Just Ruby)) (File "test/fixtures/ruby/corpus/method-declaration.B.rb"  (Just Ruby))]

        jsonOutput =  "{\"diffs\":[{\"diff\":{\"merge\":{\"after\":{\"category\":\"Program\",\"sourceRange\":[0,21],\"sourceSpan\":{\"start\":[1,1],\"end\":[4,1]}},\"children\":[{\"merge\":{\"after\":{\"category\":\"Method\",\"sourceRange\":[0,20],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,4]}},\"children\":[{\"merge\":{\"after\":{\"category\":\"Empty\",\"sourceRange\":[0,0],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,1]}},\"children\":[],\"before\":{\"category\":\"Empty\",\"sourceRange\":[0,0],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,1]}}}},{\"patch\":{\"replace\":[{\"category\":\"Identifier\",\"children\":[],\"name\":\"foo\",\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"category\":\"Identifier\",\"children\":[],\"name\":\"bar\",\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}}]}},{\"patch\":{\"insert\":{\"category\":\"Identifier\",\"children\":[],\"name\":\"a\",\"sourceRange\":[8,9],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,10]}}}},{\"merge\":{\"after\":{\"category\":\"\",\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}},\"children\":[{\"patch\":{\"insert\":{\"category\":\"Send\",\"children\":[{\"patch\":{\"insert\":{\"category\":\"Identifier\",\"children\":[],\"name\":\"baz\",\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}}}}],\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}}}}],\"before\":{\"category\":\"[]\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,4]}}}}],\"before\":{\"category\":\"Method\",\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}}}}],\"before\":{\"category\":\"Program\",\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}}}},\"stat\":{\"replace\":[{\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb\",\"language\":\"Ruby\"},{\"path\":\"test/fixtures/ruby/corpus/method-declaration.B.rb\",\"language\":\"Ruby\"}],\"path\":\"test/fixtures/ruby/corpus/method-declaration.A.rb -> test/fixtures/ruby/corpus/method-declaration.B.rb\"}}]}\n"
        sExpressionOutput = "(Program\n  (Method\n    (Empty)\n  { (Identifier)\n  ->(Identifier) }\n  {+(Identifier)+}\n    (\n    {+(Send\n      {+(Identifier)+})+})))\n"
        tocOutput = "{\"changes\":{\"test/fixtures/ruby/corpus/method-declaration.A.rb -> test/fixtures/ruby/corpus/method-declaration.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"}]},\"errors\":{}}\n"

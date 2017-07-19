{-# LANGUAGE DuplicateRecordFields #-}
module SemanticCmdLineSpec where

import Prologue
import Language
import Renderer
import SemanticCmdLine
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "runDiff" $
    for_ diffFixtures $ \ (diffRenderer, diffMode, expected) ->
      it ("renders to " <> show diffRenderer <> " in mode " <> show diffMode) $ do
        output <- runDiff diffRenderer diffMode
        output `shouldBe'` expected

  describe "runParse" $
    for_ parseFixtures $ \ (parseTreeRenderer, parseMode, expected) ->
      it ("renders to " <> show parseTreeRenderer <> " in mode " <> show parseMode) $ do
        output <- runParse parseTreeRenderer parseMode
        output `shouldBe'` expected
  where
    shouldBe' actual expected = do
      when (actual /= expected) $ print actual
      actual `shouldBe` expected

parseFixtures :: [(SomeRenderer TermRenderer, ParseMode, ByteString)]
parseFixtures =
  [ (SomeRenderer SExpressionTermRenderer, pathMode, sExpressionParseTreeOutput)
  , (SomeRenderer JSONTermRenderer, pathMode, jsonParseTreeOutput)
  , (SomeRenderer JSONTermRenderer, pathMode', jsonParseTreeOutput')
  , (SomeRenderer JSONTermRenderer, ParsePaths [], emptyJsonParseTreeOutput)
  , (SomeRenderer JSONTermRenderer, ParsePaths [("not-a-file.rb", Just Ruby)], emptyJsonParseTreeOutput)
  , (SomeRenderer ToCTermRenderer, ParsePaths [("test/fixtures/ruby/method-declaration.A.rb", Just Ruby)], tocOutput)
  ]
  where pathMode = ParsePaths [("test/fixtures/ruby/and-or.A.rb", Just Ruby)]
        pathMode' = ParsePaths [("test/fixtures/ruby/and-or.A.rb", Just Ruby), ("test/fixtures/ruby/and-or.B.rb", Just Ruby)]

        sExpressionParseTreeOutput = "(Program\n  (Binary\n    (Identifier)\n    (Other \"and\")\n    (Identifier)))\n"
        jsonParseTreeOutput = "[{\"filePath\":\"test/fixtures/ruby/and-or.A.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"Binary\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"and\",\"children\":[],\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}},\"language\":\"Ruby\"}]\n"
        jsonParseTreeOutput' = "[{\"filePath\":\"test/fixtures/ruby/and-or.A.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"Binary\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"and\",\"children\":[],\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}},\"language\":\"Ruby\"},{\"filePath\":\"test/fixtures/ruby/and-or.B.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"Binary\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"or\",\"children\":[],\"sourceRange\":[4,6],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,7]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[7,10],\"sourceSpan\":{\"start\":[1,8],\"end\":[1,11]}}],\"sourceRange\":[0,10],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,11]}},{\"category\":\"Binary\",\"children\":[{\"category\":\"Binary\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[11,12],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,2]}},{\"category\":\"or\",\"children\":[],\"sourceRange\":[13,15],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,5]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[16,17],\"sourceSpan\":{\"start\":[2,6],\"end\":[2,7]}}],\"sourceRange\":[11,17],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,7]}},{\"category\":\"and\",\"children\":[],\"sourceRange\":[18,21],\"sourceSpan\":{\"start\":[2,8],\"end\":[2,11]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[22,23],\"sourceSpan\":{\"start\":[2,12],\"end\":[2,13]}}],\"sourceRange\":[11,23],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,13]}}],\"sourceRange\":[0,24],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},\"language\":\"Ruby\"}]\n"
        emptyJsonParseTreeOutput = "[]\n"
        tocOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"foo\",\"changeType\":\"unchanged\"}]},\"errors\":{}}\n"


diffFixtures :: [(SomeRenderer DiffRenderer, DiffMode, ByteString)]
diffFixtures =
  [ (SomeRenderer PatchDiffRenderer, pathMode, patchOutput)
  , (SomeRenderer JSONDiffRenderer, pathMode, jsonOutput)
  , (SomeRenderer SExpressionDiffRenderer, pathMode, sExpressionOutput)
  , (SomeRenderer ToCDiffRenderer, pathMode, tocOutput)
  ]
  where pathMode = DiffPaths ("test/fixtures/ruby/method-declaration.A.rb", Just Ruby) ("test/fixtures/ruby/method-declaration.B.rb", Just Ruby)

        patchOutput = "diff --git a/test/fixtures/ruby/method-declaration.A.rb b/test/fixtures/ruby/method-declaration.B.rb\nindex 0000000000000000000000000000000000000000..0000000000000000000000000000000000000000 100644\n--- a/test/fixtures/ruby/method-declaration.A.rb\n+++ b/test/fixtures/ruby/method-declaration.B.rb\n@@ -1,3 +1,4 @@\n-def foo\n+def bar(a)\n+  baz\n end\n\n"

        jsonOutput = "{\"diff\":{\"after\":{\"category\":\"Program\",\"sourceRange\":[0,21],\"sourceSpan\":{\"start\":[1,1],\"end\":[4,1]}},\"children\":[{\"after\":{\"category\":\"Method\",\"identifier\":\"bar\",\"sourceRange\":[0,20],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,4]}},\"children\":[{\"replace\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}}]},{\"insert\":{\"category\":\"Params\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[8,9],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,10]}}],\"sourceRange\":[7,13],\"sourceSpan\":{\"start\":[1,8],\"end\":[2,3]}}},{\"insert\":{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}}}],\"before\":{\"category\":\"Method\",\"identifier\":\"foo\",\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}}}],\"before\":{\"category\":\"Program\",\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}}},\"oids\":[\"0000000000000000000000000000000000000000\",\"0000000000000000000000000000000000000000\"],\"paths\":[\"test/fixtures/ruby/method-declaration.A.rb\",\"test/fixtures/ruby/method-declaration.B.rb\"]}\n"
        sExpressionOutput = "(Program\n  (Method\n  { (Identifier)\n  ->(Identifier) }\n  {+(Params\n      (Identifier))+}\n  {+(Identifier)+}))\n"
        tocOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb -> test/fixtures/ruby/method-declaration.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"}]},\"errors\":{}}\n"

repo :: FilePath
repo = "test/fixtures/git/examples/all-languages.git"

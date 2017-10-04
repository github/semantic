{-# LANGUAGE DuplicateRecordFields #-}
module SemanticCmdLineSpec where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Functor.Both
import Data.Semigroup ((<>))
import Language
import Renderer
import Semantic.Task
import SemanticCmdLine
import System.IO (Handle)
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "runDiff" $
    for_ diffFixtures $ \ (diffRenderer, diffMode, expected) ->
      it ("renders to " <> show diffRenderer <> " in mode " <> show diffMode) $ do
        output <- runTask $ runDiff diffRenderer diffMode
        output `shouldBe'` expected

  describe "runParse" $
    for_ parseFixtures $ \ (parseTreeRenderer, parseMode, expected) ->
      it ("renders to " <> show parseTreeRenderer <> " in mode " <> show parseMode) $ do
        output <- runTask $ runParse parseTreeRenderer parseMode
        output `shouldBe'` expected
  where
    shouldBe' actual expected = do
      when (actual /= expected) $ print actual
      actual `shouldBe` expected

parseFixtures :: [(SomeRenderer TermRenderer, Either Handle [(FilePath, Maybe Language)], ByteString)]
parseFixtures =
  [ (SomeRenderer SExpressionTermRenderer, pathMode, sExpressionParseTreeOutput)
  , (SomeRenderer JSONTermRenderer, pathMode, jsonParseTreeOutput)
  , (SomeRenderer JSONTermRenderer, pathMode', jsonParseTreeOutput')
  , (SomeRenderer JSONTermRenderer, Right [], emptyJsonParseTreeOutput)
  , (SomeRenderer JSONTermRenderer, Right [("not-a-file.rb", Just Ruby)], emptyJsonParseTreeOutput)
  , (SomeRenderer ToCTermRenderer, Right [("test/fixtures/ruby/method-declaration.A.rb", Just Ruby)], tocOutput)
  ]
  where pathMode = Right [("test/fixtures/ruby/and-or.A.rb", Just Ruby)]
        pathMode' = Right [("test/fixtures/ruby/and-or.A.rb", Just Ruby), ("test/fixtures/ruby/and-or.B.rb", Just Ruby)]

        sExpressionParseTreeOutput = "(Program\n  (And\n    (Identifier)\n    (Identifier)))\n"
        jsonParseTreeOutput = "[{\"filePath\":\"test/fixtures/ruby/and-or.A.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"And\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}},\"language\":\"Ruby\"}]\n"
        jsonParseTreeOutput' = "[{\"filePath\":\"test/fixtures/ruby/and-or.A.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"And\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}},\"language\":\"Ruby\"},{\"filePath\":\"test/fixtures/ruby/and-or.B.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"Or\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[7,10],\"sourceSpan\":{\"start\":[1,8],\"end\":[1,11]}}],\"sourceRange\":[0,10],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,11]}},{\"category\":\"And\",\"children\":[{\"category\":\"Or\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[11,12],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,2]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[16,17],\"sourceSpan\":{\"start\":[2,6],\"end\":[2,7]}}],\"sourceRange\":[11,17],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,7]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[22,23],\"sourceSpan\":{\"start\":[2,12],\"end\":[2,13]}}],\"sourceRange\":[11,23],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,13]}}],\"sourceRange\":[0,24],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},\"language\":\"Ruby\"}]\n"
        emptyJsonParseTreeOutput = "[]\n"
        tocOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"foo\",\"changeType\":\"unchanged\"}]},\"errors\":{}}\n"


diffFixtures :: [(SomeRenderer DiffRenderer, Either Handle [Both (FilePath, Maybe Language)], ByteString)]
diffFixtures =
  [ (SomeRenderer PatchDiffRenderer, pathMode, patchOutput)
  , (SomeRenderer JSONDiffRenderer, pathMode, jsonOutput)
  , (SomeRenderer SExpressionDiffRenderer, pathMode, sExpressionOutput)
  , (SomeRenderer OldToCDiffRenderer, pathMode, tocOutput)
  ]
  where pathMode = Right [both ("test/fixtures/ruby/method-declaration.A.rb", Just Ruby) ("test/fixtures/ruby/method-declaration.B.rb", Just Ruby)]

        patchOutput = "diff --git a/test/fixtures/ruby/method-declaration.A.rb b/test/fixtures/ruby/method-declaration.B.rb\nindex 0000000000000000000000000000000000000000..0000000000000000000000000000000000000000 100644\n--- a/test/fixtures/ruby/method-declaration.A.rb\n+++ b/test/fixtures/ruby/method-declaration.B.rb\n@@ -1,3 +1,4 @@\n-def foo\n+def bar(a)\n+  baz\n end\n\n"

        jsonOutput = "{\"diff\":{\"merge\":{\"after\":{\"sourceRange\":[0,21],\"sourceSpan\":{\"start\":[1,1],\"end\":[4,1]}},\"children\":[{\"merge\":{\"after\":{\"sourceRange\":[0,20],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,4]}},\"children\":[{\"merge\":{\"after\":{\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},\"children\":[],\"before\":{\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}}}},{\"patch\":{\"replace\":[{\"children\":[],\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"children\":[],\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}}]}},{\"patch\":{\"insert\":{\"children\":[],\"sourceRange\":[8,9],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,10]}}}},{\"merge\":{\"after\":{\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}},\"children\":[{\"patch\":{\"insert\":{\"children\":[],\"sourceRange\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}}}}],\"before\":{\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[2,1],\"end\":[2,4]}}}}],\"before\":{\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}}}}],\"before\":{\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}}}},\"oids\":[\"0000000000000000000000000000000000000000\",\"0000000000000000000000000000000000000000\"],\"paths\":[\"test/fixtures/ruby/method-declaration.A.rb\",\"test/fixtures/ruby/method-declaration.B.rb\"]}\n"
        sExpressionOutput = "(Program\n  (Method\n    (Empty)\n  { (Identifier)\n  ->(Identifier) }\n  {+(Identifier)+}\n    (\n    {+(Identifier)+})))\n"
        tocOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb -> test/fixtures/ruby/method-declaration.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"}]},\"errors\":{}}\n"

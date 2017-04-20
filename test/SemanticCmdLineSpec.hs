module SemanticCmdLineSpec where

import Prologue
import Arguments
import SemanticCmdLine
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "runDiff" $ do
    it "patchDiff" $ assertDiffOutput patchDiff patchOutput
    it "splitDiff" $ assertDiffOutput splitDiff splitOutput
    it "jsonDiff" $ assertDiffOutput jsonDiff jsonOutput
    it "summaryDiff" $ assertDiffOutput summaryDiff summaryOutput
    it "sExpressionDiff" $ assertDiffOutput sExpressionDiff sExpressionOutput
    it "tocDiff" $ assertDiffOutput tocDiff tocOutput

  describe "runParse" $ do
    it "sExpressionParseTree" $ assertParseOutput sExpressionParseTree sExpressionParseTreeOutput
    it "jsonParseTree" $ assertParseOutput jsonParseTree jsonParseTreeOutput
    it "jsonIndexParseTree" $ assertParseOutput jsonIndexParseTree jsonIndexParseTreeOutput

  where
    assertDiffOutput format expected = do
      let mode = DiffPaths "test/fixtures/ruby/method-declaration.A.rb" "test/fixtures/ruby/method-declaration.B.rb"
      output <- runDiff $ format mode "" []
      when (output /= expected) $ print output  -- Helpful for debugging
      output `shouldBe` expected

    assertParseOutput format expected = do
      let mode = ParsePaths ["test/fixtures/ruby/and-or.A.rb"]
      output <- runParse $ format mode False "" []
      when (output /= expected) $ print output  -- Helpful for debugging
      output `shouldBe` expected

    patchOutput = "diff --git a/test/fixtures/ruby/method-declaration.A.rb b/test/fixtures/ruby/method-declaration.B.rb\nindex 0000000000000000000000000000000000000000..0000000000000000000000000000000000000000 100644\n--- a/test/fixtures/ruby/method-declaration.A.rb\n+++ b/test/fixtures/ruby/method-declaration.B.rb\n@@ -1,3 +1,4 @@\n-def foo\n+def bar(a)\n+  baz\n end\n\n"
    splitOutput = "<!DOCTYPE HTML>\n<html><head><link rel=\"stylesheet\" href=\"style.css\"></head><body><table class=\"diff\"><colgroup><col width=\"40\"><col><col width=\"40\"><col></colgroup><tr><td class=\"blob-num blob-num-replacement\">1</td><td class=\"blob-code blob-code-replacement\"><ul class=\"category-program\"><li><ul class=\"category-method\">def <li><div class=\"patch replace\"><span class=\"category-identifier\">foo</span></div></li>\n</ul></li></ul></td>\n<td class=\"blob-num blob-num-replacement\">1</td><td class=\"blob-code blob-code-replacement\"><ul class=\"category-program\"><li><ul class=\"category-method\">def <li><div class=\"patch replace\"><span class=\"category-identifier\">bar</span></div></li><li><div class=\"patch insert\"><ul class=\"category-parameters\">(<li><span class=\"category-identifier\">a</span></li>)\n</ul></div></li></ul></li></ul></td>\n\n</tr><tr><td class=\"blob-num blob-num-empty empty-cell\"></td><td class=\"blob-code blob-code-empty empty-cell\"></td>\n<td class=\"blob-num blob-num-replacement\">2</td><td class=\"blob-code blob-code-replacement\"><ul class=\"category-program\"><li><ul class=\"category-method\"><li><div class=\"patch insert\"><ul class=\"category-parameters\">  </ul></div></li><li><div class=\"patch insert\"><span class=\"category-identifier\">baz</span></div></li>\n</ul></li></ul></td>\n\n</tr><tr><td class=\"blob-num\">2</td><td class=\"blob-code\"><ul class=\"category-program\"><li><ul class=\"category-method\">end</ul></li>\n</ul></td>\n<td class=\"blob-num\">3</td><td class=\"blob-code\"><ul class=\"category-program\"><li><ul class=\"category-method\">end</ul></li>\n</ul></td>\n\n</tr><tr><td class=\"blob-num\">3</td><td class=\"blob-code\"><ul class=\"category-program\"></ul></td>\n<td class=\"blob-num\">4</td><td class=\"blob-code\"><ul class=\"category-program\"></ul></td>\n\n</tr></table></body></html>\n"
    summaryOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb -> test/fixtures/ruby/method-declaration.B.rb\":[{\"span\":{\"replace\":[{\"start\":[1,5],\"end\":[1,8]},{\"start\":[1,5],\"end\":[1,8]}]},\"summary\":\"Replaced the 'foo' identifier with the 'bar' identifier in the 'bar(\226\128\166)' method\"},{\"span\":{\"insert\":{\"start\":[1,9],\"end\":[1,10]}},\"summary\":\"Added the 'a' identifier in the 'bar(\226\128\166)' method\"},{\"span\":{\"insert\":{\"start\":[2,3],\"end\":[2,6]}},\"summary\":\"Added the 'baz' identifier in the 'bar(\226\128\166)' method\"}]},\"errors\":{}}\n"
    jsonOutput = "{\"oids\":[\"0000000000000000000000000000000000000000\",\"0000000000000000000000000000000000000000\"],\"paths\":[\"test/fixtures/ruby/method-declaration.A.rb\",\"test/fixtures/ruby/method-declaration.B.rb\"],\"rows\":[[{\"terms\":[{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[{\"category\":\"Identifier\",\"patch\":\"replace\",\"range\":[4,7]}],\"range\":[0,8]}],\"range\":[0,8]}],\"hasChanges\":true,\"range\":[0,8],\"number\":1},{\"terms\":[{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[{\"category\":\"Identifier\",\"patch\":\"replace\",\"range\":[4,7]},{\"category\":\"Params\",\"children\":[{\"category\":\"Identifier\",\"range\":[8,9]}],\"patch\":\"insert\",\"range\":[7,11]}],\"range\":[0,11]}],\"range\":[0,11]}],\"hasChanges\":true,\"range\":[0,11],\"number\":1}],[{\"terms\":[{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[{\"category\":\"Params\",\"children\":[],\"patch\":\"insert\",\"range\":[11,13]},{\"category\":\"Identifier\",\"patch\":\"insert\",\"range\":[13,16]}],\"range\":[11,17]}],\"range\":[11,17]}],\"hasChanges\":true,\"range\":[11,17],\"number\":2}],[{\"terms\":[{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[],\"range\":[8,11]}],\"range\":[8,12]}],\"hasChanges\":false,\"range\":[8,12],\"number\":2},{\"terms\":[{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[],\"range\":[17,20]}],\"range\":[17,21]}],\"hasChanges\":false,\"range\":[17,21],\"number\":3}],[{\"terms\":[{\"category\":\"Program\",\"children\":[],\"range\":[12,12]}],\"hasChanges\":false,\"range\":[12,12],\"number\":3},{\"terms\":[{\"category\":\"Program\",\"children\":[],\"range\":[21,21]}],\"hasChanges\":false,\"range\":[21,21],\"number\":4}]]}\n"
    sExpressionOutput = "(Program\n  (Method\n  { (Identifier)\n  ->(Identifier) }\n  {+(Params\n      (Identifier))+}\n  {+(Identifier)+}))"
    tocOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb -> test/fixtures/ruby/method-declaration.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"}]},\"errors\":{}}\n"

    sExpressionParseTreeOutput = "(Program\n  (Binary\n    (Identifier)\n    (Other \"and\")\n    (Identifier)))"
    jsonParseTreeOutput = "[{\"filePath\":\"test/fixtures/ruby/and-or.A.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"Binary\",\"children\":[{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"Other \\\"and\\\"\",\"children\":[],\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"category\":\"Identifier\",\"children\":[],\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}}},[]]\n"
    jsonIndexParseTreeOutput = "[{\"programNodes\":[{\"category\":\"Program\",\"sourceRange\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}},{\"category\":\"Binary\",\"sourceRange\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}},{\"category\":\"Identifier\",\"sourceRange\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"Other \\\"and\\\"\",\"sourceRange\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"category\":\"Identifier\",\"sourceRange\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"filePath\":\"test/fixtures/ruby/and-or.A.rb\"},[]]\n"

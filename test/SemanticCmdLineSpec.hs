{-# LANGUAGE DuplicateRecordFields #-}
module SemanticCmdLineSpec where

import Prologue
import Arguments
import SemanticCmdLine
import Data.Functor.Listable
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  prop "runDiff for all modes and formats" $
    \ DiffFixture{..} -> do
        output <- runDiff arguments
        output `shouldBe'` expected
  prop "runParse for all modes and formats" $
    \ ParseFixture{..} -> do
        output <- runParse arguments
        output `shouldBe'` expected
  where
    shouldBe' actual expected = do
      when (actual /= expected) $ print actual
      actual `shouldBe` expected


data ParseFixture = ParseFixture
  { arguments :: ParseArguments
  , expected :: ByteString
  } deriving (Show)

instance Listable ParseFixture where
  tiers = cons0 (ParseFixture (sExpressionParseTree pathMode "" []) sExpressionParseTreeOutput)
       \/ cons0 (ParseFixture (jsonParseTree False pathMode "" []) jsonParseTreeOutput)
       \/ cons0 (ParseFixture (jsonIndexParseTree False pathMode "" []) jsonIndexParseTreeOutput)
       \/ cons0 (ParseFixture (sExpressionParseTree commitMode repo []) "(Program\n  (Method\n    (Identifier)))")
       \/ cons0 (ParseFixture (jsonParseTree False commitMode repo []) "[{\"filePath\":\"methods.rb\",\"programNode\":{\"category\":\"Program\",\"start\":[1,1],\"children\":[{\"category\":\"Method\",\"start\":[1,1],\"identifier\":{\"category\":\"Identifier\",\"start\":[1,5],\"identifier\":\"foo\",\"end\":[1,8],\"range\":[4,7]},\"clauses\":[],\"receiver\":null,\"end\":[2,4],\"range\":[0,11],\"callSignature\":[],\"definitions\":[]}],\"end\":[3,1],\"range\":[0,12]}},[]]\n")
       \/ cons0 (ParseFixture (jsonIndexParseTree False commitMode repo []) "[{\"filePath\":\"methods.rb\",\"programNode\":[{\"category\":\"Method\",\"start\":[1,1],\"identifier\":\"foo\",\"end\":[2,4],\"range\":[0,11]}]},[]]\n")

    where
      pathMode = ParsePaths ["test/fixtures/ruby/and-or.A.rb"]
      commitMode = ParseCommit "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"]

      sExpressionParseTreeOutput = "(Program\n  (Binary\n    (Identifier)\n    (Other \"and\")\n    (Identifier)))"
      jsonParseTreeOutput = "[{\"filePath\":\"test/fixtures/ruby/and-or.A.rb\",\"programNode\":{\"category\":\"Program\",\"start\":[1,1],\"children\":[{\"operatorSyntaxes\":[{\"category\":\"Identifier\",\"start\":[1,1],\"identifier\":\"foo\",\"end\":[1,4],\"range\":[0,3]},{\"category\":\"and\",\"start\":[1,5],\"identifier\":\"and\",\"end\":[1,8],\"range\":[4,7]},{\"category\":\"Identifier\",\"start\":[1,9],\"identifier\":\"bar\",\"end\":[1,12],\"range\":[8,11]}],\"category\":\"Binary\",\"start\":[1,1],\"end\":[1,12],\"range\":[0,11]}],\"end\":[2,1],\"range\":[0,12]}},[]]\n"
      jsonIndexParseTreeOutput = "[{\"filePath\":\"test/fixtures/ruby/and-or.A.rb\",\"programNode\":[]},[]]\n"


data DiffFixture = DiffFixture
  { arguments :: DiffArguments
  , expected :: ByteString
  } deriving (Show)

instance Listable DiffFixture where
  tiers = cons0 (DiffFixture (patchDiff pathMode "" []) patchOutput)
       \/ cons0 (DiffFixture (splitDiff pathMode "" []) splitOutput)
       \/ cons0 (DiffFixture (jsonDiff pathMode "" []) jsonOutput)
       \/ cons0 (DiffFixture (summaryDiff pathMode "" []) summaryOutput)
       \/ cons0 (DiffFixture (sExpressionDiff pathMode "" []) sExpressionOutput)
       \/ cons0 (DiffFixture (tocDiff pathMode "" []) tocOutput)
       \/ cons0 (DiffFixture (patchDiff commitMode repo []) patchOutput')
       \/ cons0 (DiffFixture (splitDiff commitMode repo []) splitOutput')
       \/ cons0 (DiffFixture (jsonDiff commitMode repo []) jsonOutput')
       \/ cons0 (DiffFixture (summaryDiff commitMode repo []) summaryOutput')
       \/ cons0 (DiffFixture (sExpressionDiff commitMode repo []) sExpressionOutput')
       \/ cons0 (DiffFixture (tocDiff commitMode repo []) tocOutput')

    where
      pathMode = DiffPaths "test/fixtures/ruby/method-declaration.A.rb" "test/fixtures/ruby/method-declaration.B.rb"
      commitMode = DiffCommits "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"]

      patchOutput = "diff --git a/test/fixtures/ruby/method-declaration.A.rb b/test/fixtures/ruby/method-declaration.B.rb\nindex 0000000000000000000000000000000000000000..0000000000000000000000000000000000000000 100644\n--- a/test/fixtures/ruby/method-declaration.A.rb\n+++ b/test/fixtures/ruby/method-declaration.B.rb\n@@ -1,3 +1,4 @@\n-def foo\n+def bar(a)\n+  baz\n end\n\n"
      patchOutput' = "diff --git a/methods.rb b/methods.rb\nnew file mode 100644\nindex 0000000000000000000000000000000000000000..ff7bbbe9495f61d9e1e58c597502d152bab1761e\n--- /dev/null\n+++ b/methods.rb\n+def foo\n+end\n\n"
      splitOutput = "<!DOCTYPE HTML>\n<html><head><link rel=\"stylesheet\" href=\"style.css\"></head><body><table class=\"diff\"><colgroup><col width=\"40\"><col><col width=\"40\"><col></colgroup><tr><td class=\"blob-num blob-num-replacement\">1</td><td class=\"blob-code blob-code-replacement\"><ul class=\"category-program\"><li><ul class=\"category-method\">def <li><div class=\"patch replace\"><span class=\"category-identifier\">foo</span></div></li>\n</ul></li></ul></td>\n<td class=\"blob-num blob-num-replacement\">1</td><td class=\"blob-code blob-code-replacement\"><ul class=\"category-program\"><li><ul class=\"category-method\">def <li><div class=\"patch replace\"><span class=\"category-identifier\">bar</span></div></li><li><div class=\"patch insert\"><ul class=\"category-parameters\">(<li><span class=\"category-identifier\">a</span></li>)\n</ul></div></li></ul></li></ul></td>\n\n</tr><tr><td class=\"blob-num blob-num-empty empty-cell\"></td><td class=\"blob-code blob-code-empty empty-cell\"></td>\n<td class=\"blob-num blob-num-replacement\">2</td><td class=\"blob-code blob-code-replacement\"><ul class=\"category-program\"><li><ul class=\"category-method\"><li><div class=\"patch insert\"><ul class=\"category-parameters\">  </ul></div></li><li><div class=\"patch insert\"><span class=\"category-identifier\">baz</span></div></li>\n</ul></li></ul></td>\n\n</tr><tr><td class=\"blob-num\">2</td><td class=\"blob-code\"><ul class=\"category-program\"><li><ul class=\"category-method\">end</ul></li>\n</ul></td>\n<td class=\"blob-num\">3</td><td class=\"blob-code\"><ul class=\"category-program\"><li><ul class=\"category-method\">end</ul></li>\n</ul></td>\n\n</tr><tr><td class=\"blob-num\">3</td><td class=\"blob-code\"><ul class=\"category-program\"></ul></td>\n<td class=\"blob-num\">4</td><td class=\"blob-code\"><ul class=\"category-program\"></ul></td>\n\n</tr></table></body></html>\n"
      splitOutput' = "<!DOCTYPE HTML>\n<html><head><link rel=\"stylesheet\" href=\"style.css\"></head><body><table class=\"diff\"><colgroup><col width=\"40\"><col><col width=\"40\"><col></colgroup><tr><td class=\"blob-num blob-num-empty empty-cell\"></td><td class=\"blob-code blob-code-empty empty-cell\"></td>\n<td class=\"blob-num blob-num-replacement\">1</td><td class=\"blob-code blob-code-replacement\"><div class=\"patch insert\"><ul class=\"category-program\"><li><ul class=\"category-method\">def <li><span class=\"category-identifier\">foo</span></li>\n</ul></li></ul></div></td>\n\n</tr><tr><td class=\"blob-num blob-num-empty empty-cell\"></td><td class=\"blob-code blob-code-empty empty-cell\"></td>\n<td class=\"blob-num blob-num-replacement\">2</td><td class=\"blob-code blob-code-replacement\"><div class=\"patch insert\"><ul class=\"category-program\"><li><ul class=\"category-method\">end</ul></li>\n</ul></div></td>\n\n</tr><tr><td class=\"blob-num blob-num-empty empty-cell\"></td><td class=\"blob-code blob-code-empty empty-cell\"></td>\n<td class=\"blob-num blob-num-replacement\">3</td><td class=\"blob-code blob-code-replacement\"><div class=\"patch insert\"><ul class=\"category-program\"></ul></div></td>\n\n</tr></table></body></html>\n"
      summaryOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb -> test/fixtures/ruby/method-declaration.B.rb\":[{\"span\":{\"replace\":[{\"start\":[1,5],\"end\":[1,8]},{\"start\":[1,5],\"end\":[1,8]}]},\"summary\":\"Replaced the 'foo' identifier with the 'bar' identifier in the 'bar(\226\128\166)' method\"},{\"span\":{\"insert\":{\"start\":[1,9],\"end\":[1,10]}},\"summary\":\"Added the 'a' identifier in the 'bar(\226\128\166)' method\"},{\"span\":{\"insert\":{\"start\":[2,3],\"end\":[2,6]}},\"summary\":\"Added the 'baz' identifier in the 'bar(\226\128\166)' method\"}]},\"errors\":{}}\n"
      summaryOutput' = "{\"changes\":{\"methods.rb\":[{\"span\":{\"insert\":{\"start\":[1,1],\"end\":[2,4]}},\"summary\":\"Added the 'foo()' method\"}]},\"errors\":{}}\n"
      jsonOutput = "{\"oids\":[\"0000000000000000000000000000000000000000\",\"0000000000000000000000000000000000000000\"],\"paths\":[\"test/fixtures/ruby/method-declaration.A.rb\",\"test/fixtures/ruby/method-declaration.B.rb\"],\"rows\":[[{\"category\":\"Program\",\"start\":[1,1],\"children\":[{\"category\":\"Method\",\"start\":[1,1],\"children\":[{\"replace\":{\"category\":\"Identifier\",\"start\":[1,5],\"end\":[1,8],\"range\":[4,7]}}],\"end\":[2,4],\"range\":[0,8]}],\"end\":[3,1],\"range\":[0,8],\"number\":1},{\"category\":\"Program\",\"start\":[1,1],\"children\":[{\"category\":\"Method\",\"start\":[1,1],\"children\":[{\"replace\":{\"category\":\"Identifier\",\"start\":[1,5],\"end\":[1,8],\"range\":[4,7]}},{\"insert\":{\"category\":\"Params\",\"start\":[1,8],\"children\":[{\"category\":\"Identifier\",\"start\":[1,9],\"end\":[1,10],\"range\":[8,9]}],\"end\":[2,3],\"range\":[7,11]}}],\"end\":[3,4],\"range\":[0,11]}],\"end\":[4,1],\"range\":[0,11],\"number\":1}],[{\"category\":\"Program\",\"start\":[1,1],\"children\":[{\"category\":\"Method\",\"start\":[1,1],\"children\":[{\"insert\":{\"category\":\"Params\",\"start\":[1,8],\"children\":[],\"end\":[2,3],\"range\":[11,13]}},{\"insert\":{\"category\":\"Identifier\",\"start\":[2,3],\"end\":[2,6],\"range\":[13,16]}}],\"end\":[3,4],\"range\":[11,17]}],\"end\":[4,1],\"range\":[11,17],\"number\":2}],[{\"category\":\"Program\",\"start\":[1,1],\"children\":[{\"category\":\"Method\",\"start\":[1,1],\"children\":[],\"end\":[2,4],\"range\":[8,11]}],\"end\":[3,1],\"range\":[8,12],\"number\":2},{\"category\":\"Program\",\"start\":[1,1],\"children\":[{\"category\":\"Method\",\"start\":[1,1],\"children\":[],\"end\":[3,4],\"range\":[17,20]}],\"end\":[4,1],\"range\":[17,21],\"number\":3}],[{\"category\":\"Program\",\"start\":[1,1],\"children\":[],\"end\":[3,1],\"range\":[12,12],\"number\":3},{\"category\":\"Program\",\"start\":[1,1],\"children\":[],\"end\":[4,1],\"range\":[21,21],\"number\":4}]]}\n"
      jsonOutput' = "{\"oids\":[\"0000000000000000000000000000000000000000\",\"ff7bbbe9495f61d9e1e58c597502d152bab1761e\"],\"paths\":[\"methods.rb\",\"methods.rb\"],\"rows\":[[{\"insert\":{\"category\":\"Program\",\"start\":[1,1],\"children\":[{\"category\":\"Method\",\"start\":[1,1],\"children\":[{\"category\":\"Identifier\",\"start\":[1,5],\"end\":[1,8],\"range\":[4,7]}],\"end\":[2,4],\"range\":[0,8]}],\"end\":[3,1],\"range\":[0,8]},\"number\":1}],[{\"insert\":{\"category\":\"Program\",\"start\":[1,1],\"children\":[{\"category\":\"Method\",\"start\":[1,1],\"children\":[],\"end\":[2,4],\"range\":[8,11]}],\"end\":[3,1],\"range\":[8,12]},\"number\":2}],[{\"insert\":{\"category\":\"Program\",\"start\":[1,1],\"children\":[],\"end\":[3,1],\"range\":[12,12]},\"number\":3}]]}\n"
      sExpressionOutput = "(Program\n  (Method\n  { (Identifier)\n  ->(Identifier) }\n  {+(Params\n      (Identifier))+}\n  {+(Identifier)+}))"
      sExpressionOutput' = "{+(Program\n  (Method\n    (Identifier)))+}"
      tocOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb -> test/fixtures/ruby/method-declaration.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"}]},\"errors\":{}}\n"
      tocOutput' = "{\"changes\":{\"methods.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"foo\",\"changeType\":\"added\"}]},\"errors\":{}}\n"

repo :: FilePath
repo = "test/fixtures/git/examples/all-languages.git"

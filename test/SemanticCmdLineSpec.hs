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
       \/ cons0 (ParseFixture (jsonParseTree pathMode "" []) jsonParseTreeOutput)
       \/ cons0 (ParseFixture (sExpressionParseTree commitMode repo []) "(Program\n  (Method\n    (Identifier)))")
       \/ cons0 (ParseFixture (jsonParseTree commitMode repo []) "[{\"filePath\":\"methods.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"identifier\":{\"category\":\"Identifier\",\"identifier\":\"foo\",\"range\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},\"clauses\":[],\"receiver\":null,\"range\":[0,11],\"callSignature\":[],\"definitions\":[],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}}],\"range\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}}},[]]\n")

    where
      pathMode = ParsePaths ["test/fixtures/ruby/and-or.A.rb"]
      commitMode = ParseCommit "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"]

      sExpressionParseTreeOutput = "(Program\n  (Binary\n    (Identifier)\n    (Other \"and\")\n    (Identifier)))"
      jsonParseTreeOutput = "[{\"filePath\":\"test/fixtures/ruby/and-or.A.rb\",\"programNode\":{\"category\":\"Program\",\"children\":[{\"operatorSyntaxes\":[{\"category\":\"Identifier\",\"identifier\":\"foo\",\"range\":[0,3],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,4]}},{\"category\":\"and\",\"identifier\":\"and\",\"range\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}},{\"category\":\"Identifier\",\"identifier\":\"bar\",\"range\":[8,11],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,12]}}],\"category\":\"Binary\",\"range\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[1,12]}}],\"range\":[0,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,1]}}},[]]\n"


data DiffFixture = DiffFixture
  { arguments :: DiffArguments
  , expected :: ByteString
  } deriving (Show)

instance Listable DiffFixture where
  tiers = cons0 (DiffFixture (patchDiff pathMode "" []) patchOutput)
       \/ cons0 (DiffFixture (jsonDiff pathMode "" []) jsonOutput)
       \/ cons0 (DiffFixture (summaryDiff pathMode "" []) summaryOutput)
       \/ cons0 (DiffFixture (sExpressionDiff pathMode "" []) sExpressionOutput)
       \/ cons0 (DiffFixture (tocDiff pathMode "" []) tocOutput)
       \/ cons0 (DiffFixture (patchDiff commitMode repo []) patchOutput')
       \/ cons0 (DiffFixture (jsonDiff commitMode repo []) jsonOutput')
       \/ cons0 (DiffFixture (summaryDiff commitMode repo []) summaryOutput')
       \/ cons0 (DiffFixture (sExpressionDiff commitMode repo []) sExpressionOutput')
       \/ cons0 (DiffFixture (tocDiff commitMode repo []) tocOutput')

    where
      pathMode = DiffPaths "test/fixtures/ruby/method-declaration.A.rb" "test/fixtures/ruby/method-declaration.B.rb"
      commitMode = DiffCommits "dfac8fd681b0749af137aebf3203e77a06fbafc2" "2e4144eb8c44f007463ec34cb66353f0041161fe" ["methods.rb"]

      patchOutput = "diff --git a/test/fixtures/ruby/method-declaration.A.rb b/test/fixtures/ruby/method-declaration.B.rb\nindex 0000000000000000000000000000000000000000..0000000000000000000000000000000000000000 100644\n--- a/test/fixtures/ruby/method-declaration.A.rb\n+++ b/test/fixtures/ruby/method-declaration.B.rb\n@@ -1,3 +1,4 @@\n-def foo\n+def bar(a)\n+  baz\n end\n\n"
      patchOutput' = "diff --git a/methods.rb b/methods.rb\nnew file mode 100644\nindex 0000000000000000000000000000000000000000..ff7bbbe9495f61d9e1e58c597502d152bab1761e\n--- /dev/null\n+++ b/methods.rb\n+def foo\n+end\n\n"
      summaryOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb -> test/fixtures/ruby/method-declaration.B.rb\":[{\"span\":{\"replace\":[{\"start\":[1,5],\"end\":[1,8]},{\"start\":[1,5],\"end\":[1,8]}]},\"summary\":\"Replaced the 'foo' identifier with the 'bar' identifier in the 'bar(\226\128\166)' method\"},{\"span\":{\"insert\":{\"start\":[1,9],\"end\":[1,10]}},\"summary\":\"Added the 'a' identifier in the 'bar(\226\128\166)' method\"},{\"span\":{\"insert\":{\"start\":[2,3],\"end\":[2,6]}},\"summary\":\"Added the 'baz' identifier in the 'bar(\226\128\166)' method\"}]},\"errors\":{}}\n"
      summaryOutput' = "{\"changes\":{\"methods.rb\":[{\"span\":{\"insert\":{\"start\":[1,1],\"end\":[2,4]}},\"summary\":\"Added the 'foo()' method\"}]},\"errors\":{}}\n"
      jsonOutput = "{\"oids\":[\"0000000000000000000000000000000000000000\",\"0000000000000000000000000000000000000000\"],\"paths\":[\"test/fixtures/ruby/method-declaration.A.rb\",\"test/fixtures/ruby/method-declaration.B.rb\"],\"rows\":[[{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[{\"replace\":{\"category\":\"Identifier\",\"range\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}}}],\"range\":[0,8],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}}],\"range\":[0,8],\"number\":1,\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[{\"replace\":{\"category\":\"Identifier\",\"range\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}}},{\"insert\":{\"category\":\"Params\",\"children\":[{\"category\":\"Identifier\",\"range\":[8,9],\"sourceSpan\":{\"start\":[1,9],\"end\":[1,10]}}],\"range\":[7,11],\"sourceSpan\":{\"start\":[1,8],\"end\":[2,3]}}}],\"range\":[0,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,4]}}],\"range\":[0,11],\"number\":1,\"sourceSpan\":{\"start\":[1,1],\"end\":[4,1]}}],[{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[{\"insert\":{\"category\":\"Params\",\"children\":[],\"range\":[11,13],\"sourceSpan\":{\"start\":[1,8],\"end\":[2,3]}}},{\"insert\":{\"category\":\"Identifier\",\"range\":[13,16],\"sourceSpan\":{\"start\":[2,3],\"end\":[2,6]}}}],\"range\":[11,17],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,4]}}],\"range\":[11,17],\"number\":2,\"sourceSpan\":{\"start\":[1,1],\"end\":[4,1]}}],[{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[],\"range\":[8,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}}],\"range\":[8,12],\"number\":2,\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[],\"range\":[17,20],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,4]}}],\"range\":[17,21],\"number\":3,\"sourceSpan\":{\"start\":[1,1],\"end\":[4,1]}}],[{\"category\":\"Program\",\"children\":[],\"range\":[12,12],\"number\":3,\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},{\"category\":\"Program\",\"children\":[],\"range\":[21,21],\"number\":4,\"sourceSpan\":{\"start\":[1,1],\"end\":[4,1]}}]]}\n"
      jsonOutput' = "{\"oids\":[\"0000000000000000000000000000000000000000\",\"ff7bbbe9495f61d9e1e58c597502d152bab1761e\"],\"paths\":[\"methods.rb\",\"methods.rb\"],\"rows\":[[{\"insert\":{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[{\"category\":\"Identifier\",\"range\":[4,7],\"sourceSpan\":{\"start\":[1,5],\"end\":[1,8]}}],\"range\":[0,8],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}}],\"range\":[0,8],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},\"number\":1}],[{\"insert\":{\"category\":\"Program\",\"children\":[{\"category\":\"Method\",\"children\":[],\"range\":[8,11],\"sourceSpan\":{\"start\":[1,1],\"end\":[2,4]}}],\"range\":[8,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},\"number\":2}],[{\"insert\":{\"category\":\"Program\",\"children\":[],\"range\":[12,12],\"sourceSpan\":{\"start\":[1,1],\"end\":[3,1]}},\"number\":3}]]}\n"
      sExpressionOutput = "(Program\n  (Method\n  { (Identifier)\n  ->(Identifier) }\n  {+(Params\n      (Identifier))+}\n  {+(Identifier)+}))"
      sExpressionOutput' = "{+(Program\n  (Method\n    (Identifier)))+}"
      tocOutput = "{\"changes\":{\"test/fixtures/ruby/method-declaration.A.rb -> test/fixtures/ruby/method-declaration.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"}]},\"errors\":{}}\n"
      tocOutput' = "{\"changes\":{\"methods.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"foo\",\"changeType\":\"added\"}]},\"errors\":{}}\n"

repo :: FilePath
repo = "test/fixtures/git/examples/all-languages.git"

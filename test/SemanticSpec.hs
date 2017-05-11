module SemanticSpec where

import Prologue
import Semantic
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty
import Language
import Syntax
import Renderer
import Renderer.SExpression
import Source

spec :: Spec
spec = parallel $ do
  describe "parseBlob" $ do
    it "parses in the specified language" $ do
      term <- parseBlob methodsBlob
      void term `shouldBe` cofree (() :< Indexed [ cofree (() :< Method [] (cofree (() :< Leaf "foo")) Nothing [] []) ])

    it "parses line by line if not given a language" $ do
      term <- parseBlob methodsBlob { blobLanguage = Nothing }
      void term `shouldBe` cofree (() :< Indexed [ cofree (() :< Leaf "def foo\n"), cofree (() :< Leaf "end\n"), cofree (() :< Leaf "") ])

  describe "parseBlobs" $ do
    it "renders to ByteString output" $ do
      output <- parseBlobs (SExpressionParseTreeRenderer TreeOnly) [methodsBlob]
      output `shouldBe` "(Program\n  (Method\n    (Identifier)))\n"

  where
    methodsBlob = SourceBlob (Source "def foo\nend\n") "ff7bbbe9495f61d9e1e58c597502d152bab1761e" "methods.rb" (Just defaultPlainBlob) (Just Ruby)

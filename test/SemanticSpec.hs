module SemanticSpec where

import Data.Blob
import Data.Functor.Both as Both
import Language
import Patch
import Prologue
import Renderer
import Semantic
import Semantic.Task
import Syntax
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "parseBlob" $ do
    it "parses in the specified language" $ do
      Just term <- runTask $ parseBlob IdentityTermRenderer methodsBlob
      void term `shouldBe` cofree (() :< Indexed [ cofree (() :< Method [] (cofree (() :< Leaf "foo")) Nothing [] []) ])

    it "parses line by line if not given a language" $ do
      Just term <- runTask $ parseBlob IdentityTermRenderer methodsBlob { blobLanguage = Nothing }
      void term `shouldBe` cofree (() :< Indexed [ cofree (() :< Leaf "def foo\n"), cofree (() :< Leaf "end\n"), cofree (() :< Leaf "") ])

    it "renders with the specified renderer" $ do
      output <- runTask $ parseBlob SExpressionTermRenderer methodsBlob
      output `shouldBe` "(Program\n  (Method\n    (Identifier)))\n"

  describe "diffTermPair" $ do
    it "produces an Insert when the first blob is missing" $ do
      result <- runTask (diffTermPair (both (emptyBlob "/foo") (sourceBlob "/foo" Nothing "")) (runBothWith replacing) (pure (cofree (() :< []))))
      (() <$) <$> result `shouldBe` pure (Insert ())

    it "produces a Delete when the second blob is missing" $ do
      result <- runTask (diffTermPair (both (sourceBlob "/foo" Nothing "") (emptyBlob "/foo")) (runBothWith replacing) (pure (cofree (() :< []))))
      (() <$) <$> result `shouldBe` pure (Delete ())

  where
    methodsBlob = Blob "def foo\nend\n" "ff7bbbe9495f61d9e1e58c597502d152bab1761e" "methods.rb" (Just defaultPlainBlob) (Just Ruby)

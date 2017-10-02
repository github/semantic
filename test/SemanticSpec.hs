module SemanticSpec where

import Data.Blob
import Data.Diff
import Data.Functor (void)
import Data.Functor.Both as Both
import Data.Patch
import Data.Term
import Language
import Renderer
import Semantic
import Semantic.Task
import Syntax
import System.Exit
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "parseBlob" $ do
    it "parses in the specified language" $ do
      Just term <- runTask $ parseBlob IdentityTermRenderer methodsBlob
      void term `shouldBe` Term (() `In` Indexed [ Term (() `In` Method [] (Term (() `In` Leaf "foo")) Nothing [] []) ])

    it "throws if not given a language" $ do
      runTask (parseBlob SExpressionTermRenderer methodsBlob { blobLanguage = Nothing }) `shouldThrow` (\ code -> case code of
        ExitFailure 1 -> True
        _ -> False)

    it "renders with the specified renderer" $ do
      output <- runTask $ parseBlob SExpressionTermRenderer methodsBlob
      output `shouldBe` "(Program\n  (Method\n    (Empty)\n    (Identifier)\n    ([])))\n"

  describe "diffTermPair" $ do
    it "produces an Insert when the first blob is missing" $ do
      result <- runTask (diffTermPair (both (emptyBlob "/foo") (sourceBlob "/foo" Nothing "")) replacing (termIn () []) (termIn () []))
      result `shouldBe` Diff (Patch (Insert (In () [])))

    it "produces a Delete when the second blob is missing" $ do
      result <- runTask (diffTermPair (both (sourceBlob "/foo" Nothing "") (emptyBlob "/foo")) replacing (termIn () []) (termIn () []))
      result `shouldBe` Diff (Patch (Delete (In () [])))

  where
    methodsBlob = Blob "def foo\nend\n" "ff7bbbe9495f61d9e1e58c597502d152bab1761e" "methods.rb" (Just defaultPlainBlob) (Just Ruby)

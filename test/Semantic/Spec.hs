module Semantic.Spec where

import Data.Blob
import Data.Diff
import Data.Functor (void)
import Data.Functor.Both as Both
import Data.Language
import Data.Patch
import Data.Term
import Rendering.Renderer
import Semantic
import Semantic.Task
import System.Exit
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "parseBlob" $ do
    it "throws if not given a language" $ do
      runTask (parseBlob SExpressionTermRenderer methodsBlob { blobLanguage = Nothing }) `shouldThrow` (\ code -> case code of
        ExitFailure 1 -> True
        _ -> False)

    it "renders with the specified renderer" $ do
      output <- runTask $ parseBlob SExpressionTermRenderer methodsBlob
      output `shouldBe` "(Program\n  (Method\n    (Empty)\n    (Identifier)\n    ([])))\n"

  describe "diffTermPair" $ do
    it "produces an Insert when the first term is missing" $ do
      result <- runTask (diffTermPair replacing (That (termIn () [])))
      result `shouldBe` (Diff (Patch (Insert (In () []))) :: Diff [] () ())

    it "produces a Delete when the second term is missing" $ do
      result <- runTask (diffTermPair replacing (This (termIn () [])))
      result `shouldBe` (Diff (Patch (Delete (In () []))) :: Diff [] () ())

  where
    methodsBlob = Blob "def foo\nend\n" "methods.rb" (Just Ruby)

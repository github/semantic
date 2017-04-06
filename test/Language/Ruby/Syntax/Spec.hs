module Language.Ruby.Syntax.Spec where

import Data.Functor.Union
import qualified Data.Syntax.Comment as Comment
import Language.Ruby.Syntax
import Prologue
import Test.Hspec

spec :: Spec
spec = do
  describe "stepAssignment" $ do
    it "should match a comment" $ do
      stepAssignment comment [Rose (Node Comment "hello") []] `shouldBe` Just ([], wrapU (Comment.Comment "hello") :: Program Syntax ())

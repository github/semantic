module Language.Ruby.Syntax.Spec where

import Data.Functor.Union
import qualified Data.Syntax.Comment as Comment
import Language.Ruby.Syntax
import Prologue
import Test.Hspec

spec :: Spec
spec = do
  describe "stepAssignment" $ do
    it "matches nodes" $ do
      stepAssignment comment [Rose (Node Comment "hello") []] `shouldBe` Just ([], wrapU (Comment.Comment "hello") :: Program Syntax ())

    it "attempts multiple alternatives" $ do
      stepAssignment (if' <|> comment) [Rose (Node Comment "hello") []] `shouldBe` Just ([], wrapU (Comment.Comment "hello") :: Program Syntax ())

    it "matches in sequence" $ do
      stepAssignment ((,) <$> comment <*> comment) [Rose (Node Comment "hello") [], Rose (Node Comment "world") []] `shouldBe` Just ([], (wrapU (Comment.Comment "hello"), wrapU (Comment.Comment "world")) :: (Program Syntax (), Program Syntax ()))

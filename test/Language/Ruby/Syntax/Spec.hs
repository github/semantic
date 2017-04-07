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
      stepAssignment comment [makeCommentAST "hello"] `shouldBe` Just ([], wrapU (Comment.Comment "hello") :: Program Syntax ())

    it "attempts multiple alternatives" $ do
      stepAssignment (if' <|> comment) [makeCommentAST "hello"] `shouldBe` Just ([], wrapU (Comment.Comment "hello") :: Program Syntax ())

    it "matches in sequence" $ do
      stepAssignment ((,) <$> comment <*> comment) [makeCommentAST "hello", makeCommentAST "world"] `shouldBe` Just ([], (wrapU (Comment.Comment "hello"), wrapU (Comment.Comment "world")) :: (Program Syntax (), Program Syntax ()))

makeCommentAST :: ByteString -> AST Grammar
makeCommentAST s = Rose (Node Comment s) []

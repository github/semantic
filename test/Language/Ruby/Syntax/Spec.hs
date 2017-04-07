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
      stepAssignment comment [ast Comment "hello" []] `shouldBe` Just ([], wrapU (Comment.Comment "hello") :: Program Syntax ())

    it "attempts multiple alternatives" $ do
      stepAssignment (if' <|> comment) [ast Comment "hello" []] `shouldBe` Just ([], wrapU (Comment.Comment "hello") :: Program Syntax ())

    it "matches in sequence" $ do
      stepAssignment ((,) <$> comment <*> comment) [ast Comment "hello" [], ast Comment "world" []] `shouldBe` Just ([], (wrapU (Comment.Comment "hello"), wrapU (Comment.Comment "world")) :: (Program Syntax (), Program Syntax ()))

ast :: Grammar -> ByteString -> [AST Grammar] -> AST Grammar
ast g s c = Rose (Node g s) c

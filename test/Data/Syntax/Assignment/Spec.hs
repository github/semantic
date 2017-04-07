module Data.Syntax.Assignment.Spec where

import Data.Syntax.Assignment
import Prologue
import Test.Hspec

spec :: Spec
spec = do
  describe "runAssignment" $ do
    it "matches nodes" $
      runAssignment red [ast Red "hello" []] `shouldBe` Just ([], Out "hello")

    it "attempts multiple alternatives" $
      runAssignment (green <|> red) [ast Red "hello" []] `shouldBe` Just ([], Out "hello")

    it "matches in sequence" $
      runAssignment ((,) <$> red <*> red) [ast Red "hello" [], ast Red "world" []] `shouldBe` Just ([], (Out "hello", Out "world"))

    it "matches repetitions" $
      runAssignment (many red) [ast Red "colourless" [], ast Red "green" [], ast Red "ideas" [], ast Red "sleep" [], ast Red "furiously" []] `shouldBe` Just ([], [Out "colourless", Out "green", Out "ideas", Out "sleep", Out "furiously"])

    it "matches one-or-more repetitions against one or more input nodes" $
      runAssignment (some red) [ast Red "hello" []] `shouldBe` Just ([], [Out "hello"])

  describe "children" $ do
    it "advances past the current node" $
      fst <$> runAssignment (children (pure (Out ""))) [ast Red "a" []] `shouldBe` Just []

    it "matches if its subrule matches" $
      () <$ runAssignment (children red) [ast Blue "b" [ast Red "a" []]] `shouldBe` Just ()

ast :: Grammar -> ByteString -> [AST Grammar] -> AST Grammar
ast g s c = Rose (Node g s) c

data Grammar = Red | Green | Blue
  deriving (Eq, Show)

data Out = Out ByteString
  deriving (Eq, Show)

red :: Assignment Grammar Out
red = Out <$ rule Red <*> content

green :: Assignment Grammar Out
green = Out <$ rule Green <*> content

blue :: Assignment Grammar Out
blue = Out <$ rule Blue <*> content

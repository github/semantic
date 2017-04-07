module Data.Syntax.Assignment.Spec where

import Data.Syntax.Assignment
import Prologue
import Test.Hspec

spec :: Spec
spec = do
  describe "stepAssignment" $ do
    it "matches nodes" $
      stepAssignment red [ast Red "hello" []] `shouldBe` Just ([], Out "hello")

    it "attempts multiple alternatives" $
      stepAssignment (green <|> red) [ast Red "hello" []] `shouldBe` Just ([], Out "hello")

    it "matches in sequence" $
      stepAssignment ((,) <$> red <*> red) [ast Red "hello" [], ast Red "world" []] `shouldBe` Just ([], (Out "hello", Out "world"))

    it "matches repetitions" $
      stepAssignment (many red) [ast Red "colourless" [], ast Red "green" [], ast Red "ideas" [], ast Red "sleep" [], ast Red "furiously" []] `shouldBe` Just ([], [Out "colourless", Out "green", Out "ideas", Out "sleep", Out "furiously"])

    it "matches one-or-more repetitions against one or more input nodes" $
      stepAssignment (some red) [ast Red "hello" []] `shouldBe` Just ([], [Out "hello"])

ast :: Grammar -> ByteString -> [AST Grammar] -> AST Grammar
ast g s c = Rose (Node g s) c

data Grammar = Red | Green | Blue
  deriving (Eq, Show)

data Out = Out ByteString
  deriving (Eq, Show)

red :: Assignment Grammar Out
red = rule Red (Out <$> content)

green :: Assignment Grammar Out
green = rule Green (Out <$> content)

blue :: Assignment Grammar Out
blue = rule Blue (Out <$> content)

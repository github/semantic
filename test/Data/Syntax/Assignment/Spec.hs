module Data.Syntax.Assignment.Spec where

import Data.ByteString.Char8 (words)
import Data.Syntax.Assignment
import Prologue
import Test.Hspec

spec :: Spec
spec = do
  describe "Applicative" $ do
    it "matches in sequence" $
      runAssignment ((,) <$> red <*> red) [ast Red "hello" [], ast Red "world" []] `shouldBe` Just ([], (Out "hello", Out "world"))

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      runAssignment (green <|> red) [ast Red "hello" []] `shouldBe` Just ([], Out "hello")

    it "matches repetitions" $
      let w = words "colourless green ideas sleep furiously" in
      runAssignment (many red) (flip (ast Red) [] <$> w) `shouldBe` Just ([], Out <$> w)

    it "matches one-or-more repetitions against one or more input nodes" $
      runAssignment (some red) [ast Red "hello" []] `shouldBe` Just ([], [Out "hello"])

  describe "rule" $ do
    it "matches nodes with the same symbol" $
      runAssignment red [ast Red "hello" []] `shouldBe` Just ([], Out "hello")

    it "does not advance past the current node" $
      fst <$> runAssignment (rule ()) [ Rose (Node () "hi") [] ] `shouldBe` Just [ Rose (Node () "hi") [] ]

  describe "content" $ do
    it "produces the node’s content" $
      snd <$> runAssignment content [ Rose (Node () "hi") [] ] `shouldBe` Just "hi"

    it "advances past the current node" $
      fst <$> runAssignment content [ Rose (Node () "hi") [] ] `shouldBe` Just []

  describe "children" $ do
    it "advances past the current node" $
      fst <$> runAssignment (children (pure (Out ""))) [ast Red "a" []] `shouldBe` Just []

    it "matches if its subrule matches" $
      () <$ runAssignment (children red) [ast Blue "b" [ast Red "a" []]] `shouldBe` Just ()

    it "does not match if its subrule does not match" $
      runAssignment (children red) [ast Blue "b" [ast Green "a" []]] `shouldBe` Nothing

ast :: grammar -> ByteString -> [AST grammar] -> AST grammar
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

module Data.Syntax.Assignment.Spec where

import Data.ByteString.Char8 (words)
import Data.Syntax.Assignment
import Prologue
import Test.Hspec
import Text.Parser.TreeSitter.Language (Symbol(..), SymbolType(..))

spec :: Spec
spec = do
  describe "Applicative" $ do
    it "matches in sequence" $
      runAssignment ((,) <$> red <*> red) [ast Red "hello" [], ast Red "world" []] `shouldBe` Result ([], (Out "hello", Out "world"))

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      runAssignment (green <|> red) [ast Red "hello" []] `shouldBe` Result ([], Out "hello")

    it "matches repetitions" $
      let w = words "colourless green ideas sleep furiously" in
      runAssignment (many red) (flip (ast Red) [] <$> w) `shouldBe` Result ([], Out <$> w)

    it "matches one-or-more repetitions against one or more input nodes" $
      runAssignment (some red) [ast Red "hello" []] `shouldBe` Result ([], [Out "hello"])

  describe "rule" $ do
    it "matches nodes with the same symbol" $
      runAssignment red [ast Red "hello" []] `shouldBe` Result ([], Out "hello")

    it "does not advance past the current node" $
      fst <$> runAssignment (rule Red) [ Rose (Node Red "hi") [] ] `shouldBe` Result [ Rose (Node Red "hi") [] ]

  describe "content" $ do
    it "produces the node’s content" $
      snd <$> runAssignment content [ Rose (Node Red "hi") [] ] `shouldBe` Result "hi"

    it "advances past the current node" $
      fst <$> runAssignment content [ Rose (Node Red "hi") [] ] `shouldBe` Result []

  describe "children" $ do
    it "advances past the current node" $
      fst <$> runAssignment (children (pure (Out ""))) [ast Red "a" []] `shouldBe` Result []

    it "matches if its subrule matches" $
      () <$ runAssignment (children red) [ast Blue "b" [ast Red "a" []]] `shouldBe` Result ()

    it "does not match if its subrule does not match" $
      runAssignment (children red) [ast Blue "b" [ast Green "a" []]] `shouldBe` Error []

    it "matches nested children" $ do
      runAssignment
        (rule Red *> children (rule Green *> children (rule Blue *> content)))
        [ ast Red "" [ ast Green "" [ ast Blue "1" [] ] ] ]
      `shouldBe`
        Result ([], "1")

    it "continues after children" $ do
      runAssignment
        (many (rule Red *> children (rule Green *> content)
           <|> rule Blue *> content))
        [ ast Red "" [ ast Green "B" [] ]
        , ast Blue "C" [] ]
      `shouldBe`
        Result ([], ["B", "C"])

    it "matches multiple nested children" $ do
      runAssignment
        (rule Red *> children (many (rule Green *> children (rule Blue *> content))))
        [ ast Red "" [ ast Green "" [ ast Blue "1" [] ]
                     , ast Green "" [ ast Blue "2" [] ] ] ]
      `shouldBe`
        Result ([], ["1", "2"])

ast :: grammar -> ByteString -> [AST grammar] -> AST grammar
ast g s c = Rose (Node g s) c

data Grammar = Red | Green | Blue
  deriving (Eq, Show)

instance Symbol Grammar where
  symbolType _ = Regular

data Out = Out ByteString
  deriving (Eq, Show)

red :: Assignment Grammar Out
red = Out <$ rule Red <*> content

green :: Assignment Grammar Out
green = Out <$ rule Green <*> content

blue :: Assignment Grammar Out
blue = Out <$ rule Blue <*> content

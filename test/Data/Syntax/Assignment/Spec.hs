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

  describe "symbol" $ do
    it "matches nodes with the same symbol" $
      runAssignment red [ast Red "hello" []] `shouldBe` Result ([], Out "hello")

    it "does not advance past the current node" $
      fst <$> runAssignment (symbol Red) [ Rose (Node Red "hi") [] ] `shouldBe` Result [ Rose (Node Red "hi") [] ]

  describe "source" $ do
    it "produces the node’s source" $
      snd <$> runAssignment source [ Rose (Node Red "hi") [] ] `shouldBe` Result "hi"

    it "advances past the current node" $
      fst <$> runAssignment source [ Rose (Node Red "hi") [] ] `shouldBe` Result []

  describe "children" $ do
    it "advances past the current node" $
      fst <$> runAssignment (children (pure (Out ""))) [ast Red "a" []] `shouldBe` Result []

    it "matches if its subrule matches" $
      () <$ runAssignment (children red) [ast Blue "b" [ast Red "a" []]] `shouldBe` Result ()

    it "does not match if its subrule does not match" $
      runAssignment (children red) [ast Blue "b" [ast Green "a" []]] `shouldBe` Error []

    it "matches nested children" $ do
      runAssignment
        (symbol Red *> children (symbol Green *> children (symbol Blue *> source)))
        [ ast Red "" [ ast Green "" [ ast Blue "1" [] ] ] ]
      `shouldBe`
        Result ([], "1")

    it "continues after children" $ do
      runAssignment
        (many (symbol Red *> children (symbol Green *> source)
           <|> symbol Blue *> source))
        [ ast Red "" [ ast Green "B" [] ]
        , ast Blue "C" [] ]
      `shouldBe`
        Result ([], ["B", "C"])

    it "matches multiple nested children" $ do
      runAssignment
        (symbol Red *> children (many (symbol Green *> children (symbol Blue *> source))))
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
red = Out <$ symbol Red <*> source

green :: Assignment Grammar Out
green = Out <$ symbol Green <*> source

blue :: Assignment Grammar Out
blue = Out <$ symbol Blue <*> source

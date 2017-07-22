{-# LANGUAGE DataKinds #-}
module Data.Syntax.Assignment.Spec where

import Data.ByteString.Char8 as B (words, length)
import Data.Source
import Data.Syntax.Assignment
import Info
import Prologue
import Test.Hspec
import Text.Parser.TreeSitter.Language (Symbol(..), SymbolType(..))

spec :: Spec
spec = do
  describe "Applicative" $
    it "matches in sequence" $
      fst <$> runAssignment "helloworld" headF ((,) <$> red <*> red) (makeState [node Red 0 5 [], node Red 5 10 []])
      `shouldBe`
        Right (Out "hello", Out "world")

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      fst <$> runAssignment "hello" headF (green <|> red) (makeState [node Red 0 5 []])
      `shouldBe`
        Right (Out "hello")

    it "matches repetitions" $
      let s = "colourless green ideas sleep furiously"
          w = words s
          (_, nodes) = foldl (\ (i, prev) word -> (i + B.length word + 1, prev <> [node Red i (i + B.length word) []])) (0, []) w in
      fst <$> runAssignment (fromBytes s) headF (many red) (makeState nodes)
      `shouldBe`
        Right (Out <$> w)

    it "matches one-or-more repetitions against one or more input nodes" $
      fst <$> runAssignment "hello" headF (some red) (makeState [node Red 0 5 []])
      `shouldBe`
        Right [Out "hello"]

  describe "symbol" $ do
    it "matches nodes with the same symbol" $
      fst <$> runAssignment "hello" headF red (makeState [node Red 0 5 []]) `shouldBe` Right (Out "hello")

    it "does not advance past the current node" $
      let initialState = makeState [ node Red 0 2 [] ] in
      snd <$> runAssignment "hi" headF (symbol Red) initialState `shouldBe` Right initialState

  describe "without catchError" $ do
    it "assignment returns UnexpectedSymbol" $
      runAssignment "A" headF
        red
        (makeState [node Green 0 1 []])
        `shouldBe`
          Left (Error (Info.Pos 1 1) (UnexpectedSymbol [Red] Green))

    it "assignment returns UnexpectedEndOfInput" $
      runAssignment "A" headF
        (symbol Green *> children (some red))
        (makeState [node Green 0 1 []])
        `shouldBe`
          Left (Error (Info.Pos 1 1) (UnexpectedEndOfInput [Red]))

  describe "catchError" $ do
    it "handler that always matches" $
      fst <$> runAssignment "A" headF
        (red `catchError` (\ _ -> OutError <$ location <*> source))
        (makeState [node Green 0 1 []])
        `shouldBe`
          Right (OutError "A")

    it "handler that matches" $
      fst <$> runAssignment "A" headF
        (red `catchError` const green)
        (makeState [node Green 0 1 []])
        `shouldBe`
          Right (Out "A")

    it "handler that doesn't match produces error" $
      runAssignment "A" headF
        (red `catchError` const blue)
        (makeState [node Green 0 1 []])
        `shouldBe`
          Left (Error (Info.Pos 1 1) (UnexpectedSymbol [Blue] Green))

    describe "in many" $ do
      it "handler that always matches" $
        fst <$> runAssignment "PG" headF
          (symbol Palette *> children (
            many (red `catchError` (\ _ -> OutError <$ location <*> source))
          ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            Right [OutError "G"]

      it "handler that matches" $
        fst <$> runAssignment "PG" headF
          (symbol Palette *> children ( many (red `catchError` const green) ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            Right [Out "G"]

      it "handler that doesn't match produces error" $
        runAssignment "PG" headF
          (symbol Palette *> children ( many (red `catchError` const blue) ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            Left (Error (Info.Pos 1 2) (UnexpectedSymbol [Blue] Green))

      it "handler that always matches with apply consumes and then errors" $
        runAssignment "PG" headF
          (symbol Palette *> children (
            (,) <$> many (red `catchError` (\ _ -> OutError <$ location <*> source)) <*> green
          ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            Left (Error (Info.Pos 1 3) (UnexpectedEndOfInput [Green]))

      it "handler that doesn't match with apply" $
        fst <$> runAssignment "PG" headF
          (symbol Palette *> children (
            (,) <$> many (red `catchError` const blue) <*> green
          ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            Right ([], Out "G")

  describe "many" $ do
    it "takes ones and only one zero width repetition" $
      fst <$> runAssignment "PGG" headF
        (symbol Palette *> children ( many (green <|> pure (Out "always")) ))
        (makeState [node Palette 0 1 [node Green 1 2 [], node Green 2 3 []]])
        `shouldBe`
          Right [Out "G", Out "G", Out "always"]

  describe "source" $ do
    it "produces the node’s source" $
      assignBy headF source "hi" (node Red 0 2 []) `shouldBe` Right "hi"

    it "advances past the current node" $
      snd <$> runAssignment "hi" headF source (makeState [ node Red 0 2 [] ])
      `shouldBe`
        Right (AssignmentState 2 (Info.Pos 1 3) Nothing 1 [])

  describe "children" $ do
    it "advances past the current node" $
      snd <$> runAssignment "a" headF (children (pure (Out ""))) (makeState [node Red 0 1 []])
      `shouldBe`
        Right (AssignmentState 1 (Info.Pos 1 2) Nothing 1 [])

    it "matches if its subrule matches" $
      () <$ runAssignment "a" headF (children red) (makeState [node Blue 0 1 [node Red 0 1 []]])
      `shouldBe`
        Right ()

    it "does not match if its subrule does not match" $
      runAssignment "a" headF (children red) (makeState [node Blue 0 1 [node Green 0 1 []]])
      `shouldBe`
        Left (Error (Info.Pos 1 1) (UnexpectedSymbol [Red] Green))

    it "matches nested children" $
      fst <$> runAssignment "1" headF
        (symbol Red *> children (symbol Green *> children (symbol Blue *> source)))
        (makeState [ node Red 0 1 [ node Green 0 1 [ node Blue 0 1 [] ] ] ])
      `shouldBe`
        Right "1"

    it "continues after children" $
      fst <$> runAssignment "BC" headF
        (many (symbol Red *> children (symbol Green *> source)
           <|> symbol Blue *> source))
        (makeState [ node Red 0 1 [ node Green 0 1 [] ]
                   , node Blue 1 2 [] ])
      `shouldBe`
        Right ["B", "C"]

    it "matches multiple nested children" $
      fst <$> runAssignment "12" headF
        (symbol Red *> children (many (symbol Green *> children (symbol Blue *> source))))
        (makeState [ node Red 0 2 [ node Green 0 1 [ node Blue 0 1 [] ]
                                  , node Green 1 2 [ node Blue 1 2 [] ] ] ])
      `shouldBe`
        Right ["1", "2"]

  describe "runAssignment" $ do
    it "drops anonymous nodes before matching symbols" $
      fst <$> runAssignment "magenta red" headF red (makeState [node Magenta 0 7 [], node Red 8 11 []])
      `shouldBe`
        Right (Out "red")

    it "does not drop anonymous nodes after matching" $
      stateNodes . snd <$> runAssignment "red magenta" headF red (makeState [node Red 0 3 [], node Magenta 4 11 []])
      `shouldBe`
        Right [node Magenta 4 11 []]

    it "does not drop anonymous nodes when requested" $
      fst <$> runAssignment "magenta red" headF ((,) <$> magenta <*> red) (makeState [node Magenta 0 7 [], node Red 8 11 []])
      `shouldBe`
        Right (Out "magenta", Out "red")

node :: symbol -> Int -> Int -> [AST symbol] -> AST symbol
node symbol start end children = cofree $ Node symbol (Range start end) (Info.Span (Info.Pos 1 (succ start)) (Info.Pos 1 (succ end))) :< children

data Grammar = Palette | Red | Green | Blue | Magenta
  deriving (Enum, Eq, Show)

instance Symbol Grammar where
  symbolType Magenta = Anonymous
  symbolType _ = Regular

data Out = Out ByteString | OutError ByteString
  deriving (Eq, Show)

red :: Assignment (AST Grammar) Grammar Out
red = Out <$ symbol Red <*> source

green :: Assignment (AST Grammar) Grammar Out
green = Out <$ symbol Green <*> source

blue :: Assignment (AST Grammar) Grammar Out
blue = Out <$ symbol Blue <*> source

magenta :: Assignment (AST Grammar) Grammar Out
magenta = Out <$ symbol Magenta <*> source

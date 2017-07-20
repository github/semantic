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
      fst <$> runAssignment headF ((,) <$> red <*> red) (makeState "helloworld" [node Red 0 5 [], node Red 5 10 []])
      `shouldBe`
        Right (Out "hello", Out "world")

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      fst <$> runAssignment headF (green <|> red) (makeState "hello" [node Red 0 5 []])
      `shouldBe`
        Right (Out "hello")

    it "matches repetitions" $
      let s = "colourless green ideas sleep furiously"
          w = words s
          (_, nodes) = foldl (\ (i, prev) word -> (i + B.length word + 1, prev <> [node Red i (i + B.length word) []])) (0, []) w in
      fst <$> runAssignment headF (many red) (makeState (fromBytes s) nodes)
      `shouldBe`
        Right (Out <$> w)

    it "matches one-or-more repetitions against one or more input nodes" $
      fst <$> runAssignment headF (some red) (makeState "hello" [node Red 0 5 []])
      `shouldBe`
        Right [Out "hello"]

  describe "symbol" $ do
    it "matches nodes with the same symbol" $
      fst <$> runAssignment headF red (makeState "hello" [node Red 0 5 []]) `shouldBe` Right (Out "hello")

    it "does not advance past the current node" $
      let initialState = makeState "hi" [ node Red 0 2 [] ] in
      snd <$> runAssignment headF (symbol Red) initialState `shouldBe` Right initialState

  describe "without catchError" $ do
    it "assignment returns UnexpectedSymbol" $
      runAssignment headF
        red
        (makeState "A" [node Green 0 1 []])
        `shouldBe`
          Left (Error (Info.Pos 1 1) (UnexpectedSymbol [Red] Green))

    it "assignment returns UnexpectedEndOfInput" $
      runAssignment headF
        (symbol Green *> children (some red))
        (makeState "A" [node Green 0 1 []])
        `shouldBe`
          Left (Error (Info.Pos 1 1) (UnexpectedEndOfInput [Red]))

  describe "catchError" $ do
    it "handler that always matches" $
      fst <$> runAssignment headF
        (red `catchError` (\ _ -> OutError <$ location <*> source))
        (makeState "A" [node Green 0 1 []])
        `shouldBe`
          Right (OutError "A")

    it "handler that matches" $
      fst <$> runAssignment headF
        (red `catchError` const green)
        (makeState "A" [node Green 0 1 []])
        `shouldBe`
          Right (Out "A")

    it "handler that doesn't match produces error" $
      runAssignment headF
        (red `catchError` const blue)
        (makeState "A" [node Green 0 1 []])
        `shouldBe`
          Left (Error (Info.Pos 1 1) (UnexpectedSymbol [Blue] Green))

    describe "in many" $ do
      let err = Just (Error (Info.Pos 1 3) (UnexpectedEndOfInput []))
      it "handler that always matches" $
        fst <$> runAssignment headF
          (symbol Palatte *> children (
            many (red `catchError` (\ _ -> OutError <$ location <*> source))
          ))
          (makeState "PG" [node Palatte 0 1 [node Green 1 2 []]])
          `shouldBe`
            Right [OutError "G"]

      let err = Just (Error (Info.Pos 1 3) (UnexpectedEndOfInput [Green]))
      it "handler that matches" $
        fst <$> runAssignment headF
          (symbol Palatte *> children ( many (red `catchError` const green) ))
          (makeState "PG" [node Palatte 0 1 [node Green 1 2 []]])
          `shouldBe`
            Right [Out "G"]

      it "handler that doesn't match produces error" $
        runAssignment headF
          (symbol Palatte *> children ( many (red `catchError` const blue) ))
          (makeState "PG" [node Palatte 0 1 [node Green 1 2 []]])
          `shouldBe`
            Left (Error (Info.Pos 1 2) (UnexpectedSymbol [Blue] Green))

      it "handler that always matches with apply consumes and then errors" $
        runAssignment headF
          (symbol Palatte *> children (
            (,) <$> many (red `catchError` (\ _ -> OutError <$ location <*> source)) <*> green
          ))
          (makeState "PG" [node Palatte 0 1 [node Green 1 2 []]])
          `shouldBe`
            Left (Error (Info.Pos 1 3) (UnexpectedEndOfInput [Green]))

      let err = Just (Error (Info.Pos 1 2) (UnexpectedSymbol [Blue] Green))
      it "handler that doesn't match with apply" $
        fst <$> runAssignment headF
          (symbol Palatte *> children (
            (,) <$> many (red `catchError` const blue) <*> green
          ))
          (makeState "PG" [node Palatte 0 1 [node Green 1 2 []]])
          `shouldBe`
            Right ([], Out "G")

  describe "many" $ do
    let err = Just (Error (Info.Pos 1 2) (UnexpectedEndOfInput [Green]))
    it "takes ones and only one zero width repetition" $
      fst <$> runAssignment headF
        (symbol Palatte *> children ( many (green <|> pure (Out "always")) ))
        (makeState "PGG" [node Palatte 0 1 [node Green 1 2 [], node Green 2 3 []]])
        `shouldBe`
          Right [Out "G", Out "G", Out "always"]

  describe "source" $ do
    it "produces the node’s source" $
      assignBy headF source "hi" (node Red 0 2 []) `shouldBe` Right "hi"

    it "advances past the current node" $
      snd <$> runAssignment headF source (makeState "hi" [ node Red 0 2 [] ]) `shouldBe` Right (AssignmentState 2 (Info.Pos 1 3) Nothing 1 "hi" [])

  describe "children" $ do
    it "advances past the current node" $
      snd <$> runAssignment headF (children (pure (Out ""))) (makeState "a" [node Red 0 1 []]) `shouldBe` Right (AssignmentState 1 (Info.Pos 1 2) Nothing 1 "a" [])

    it "matches if its subrule matches" $
      () <$ runAssignment headF (children red) (makeState "a" [node Blue 0 1 [node Red 0 1 []]]) `shouldBe` Right ()

    it "does not match if its subrule does not match" $
      runAssignment headF (children red) (makeState "a" [node Blue 0 1 [node Green 0 1 []]]) `shouldBe` Left (Error (Info.Pos 1 1) (UnexpectedSymbol [Red] Green))

    it "matches nested children" $
      fst <$> runAssignment headF
        (symbol Red *> children (symbol Green *> children (symbol Blue *> source)))
        (makeState "1" [ node Red 0 1 [ node Green 0 1 [ node Blue 0 1 [] ] ] ])
      `shouldBe`
        Right "1"

    it "continues after children" $
      fst <$> runAssignment headF
        (many (symbol Red *> children (symbol Green *> source)
           <|> symbol Blue *> source))
        (makeState "BC" [ node Red 0 1 [ node Green 0 1 [] ]
                        , node Blue 1 2 [] ])
      `shouldBe`
        Right ["B", "C"]

    it "matches multiple nested children" $
      fst <$> runAssignment headF
        (symbol Red *> children (many (symbol Green *> children (symbol Blue *> source))))
        (makeState "12" [ node Red 0 2 [ node Green 0 1 [ node Blue 0 1 [] ]
                                       , node Green 1 2 [ node Blue 1 2 [] ] ] ])
      `shouldBe`
        Right ["1", "2"]

  describe "runAssignment" $ do
    it "drops anonymous nodes before matching symbols" $
      fst <$> runAssignment headF red (makeState "magenta red" [node Magenta 0 7 [], node Red 8 11 []])
      `shouldBe`
        Right (Out "red")

    it "does not drop anonymous nodes after matching" $
      stateNodes . snd <$> runAssignment headF red (makeState "red magenta" [node Red 0 3 [], node Magenta 4 11 []])
      `shouldBe`
        Right [node Magenta 4 11 []]

    it "does not drop anonymous nodes when requested" $
      fst <$> runAssignment headF ((,) <$> magenta <*> red) (makeState "magenta red" [node Magenta 0 7 [], node Red 8 11 []])
      `shouldBe`
        Right (Out "magenta", Out "red")

node :: symbol -> Int -> Int -> [AST symbol] -> AST symbol
node symbol start end children = cofree $ Node symbol (Range start end) (Info.Span (Info.Pos 1 (succ start)) (Info.Pos 1 (succ end))) :< children

data Grammar = Palatte | Red | Green | Blue | Magenta
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

{-# LANGUAGE DataKinds #-}
module Data.Syntax.Assignment.Spec where

import Data.ByteString.Char8 as B (words, length)
import Data.Record
import Data.Syntax.Assignment
import Info
import Prologue
import Source hiding (source, length)
import Test.Hspec
import Text.Parser.TreeSitter.Language (Symbol(..), SymbolType(..))

spec :: Spec
spec = do
  describe "Applicative" $
    it "matches in sequence" $
      runAssignment headF ((,) <$> red <*> red) (makeState "helloworld" [node Red 0 5 [], node Red 5 10 []]) `shouldBe` Result Nothing (Just ((Out "hello", Out "world"), AssignmentState 10 (Info.Pos 1 11) "" []))

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      runAssignment headF (green <|> red) (makeState "hello" [node Red 0 5 []]) `shouldBe` Result Nothing (Just (Out "hello", AssignmentState 5 (Info.Pos 1 6) "" []))

    it "matches repetitions" $
      let s = "colourless green ideas sleep furiously"
          w = words s
          (_, nodes) = foldl (\ (i, prev) word -> (i + B.length word + 1, prev <> [node Red i (i + B.length word) []])) (0, []) w in
      resultValue (runAssignment headF (many red) (makeState (Source s) nodes)) `shouldBe` Just (Out <$> w, AssignmentState (B.length s) (Info.Pos 1 (succ (B.length s))) "" [])

    it "matches one-or-more repetitions against one or more input nodes" $
      resultValue (runAssignment headF (some red) (makeState "hello" [node Red 0 5 []])) `shouldBe` Just ([Out "hello"], AssignmentState 5 (Info.Pos 1 6) "" [])

  describe "symbol" $ do
    it "matches nodes with the same symbol" $
      fst <$> runAssignment headF red (makeState "hello" [node Red 0 5 []]) `shouldBe` Result Nothing (Just (Out "hello"))

    it "does not advance past the current node" $
      let initialState = makeState "hi" [ node Red 0 2 [] ] in
      snd <$> runAssignment headF (symbol Red) initialState `shouldBe` Result Nothing (Just initialState)

  describe "source" $ do
    it "produces the node’s source" $
      assignBy headF source "hi" (node Red 0 2 []) `shouldBe` Result Nothing (Just "hi")

    it "advances past the current node" $
      snd <$> runAssignment headF source (makeState "hi" [ node Red 0 2 [] ]) `shouldBe` Result Nothing (Just (AssignmentState 2 (Info.Pos 1 3) "" []))

  describe "children" $ do
    it "advances past the current node" $
      snd <$> runAssignment headF (children (pure (Out ""))) (makeState "a" [node Red 0 1 []]) `shouldBe` Result Nothing (Just (AssignmentState 1 (Info.Pos 1 2) "" []))

    it "matches if its subrule matches" $
      () <$ runAssignment headF (children red) (makeState "a" [node Blue 0 1 [node Red 0 1 []]]) `shouldBe` Result Nothing (Just ())

    it "does not match if its subrule does not match" $
      (runAssignment headF (children red) (makeState "a" [node Blue 0 1 [node Green 0 1 []]])) `shouldBe` Result (Just (Error (Info.Pos 1 1) (UnexpectedSymbol [Red] Green))) Nothing

    it "matches nested children" $
      runAssignment headF
        (symbol Red *> children (symbol Green *> children (symbol Blue *> source)))
        (makeState "1" [ node Red 0 1 [ node Green 0 1 [ node Blue 0 1 [] ] ] ])
      `shouldBe`
        Result Nothing (Just ("1", AssignmentState 1 (Info.Pos 1 2) "" []))

    it "continues after children" $
      resultValue (runAssignment headF
        (many (symbol Red *> children (symbol Green *> source)
           <|> symbol Blue *> source))
        (makeState "BC" [ node Red 0 1 [ node Green 0 1 [] ]
                        , node Blue 1 2 [] ]))
      `shouldBe`
        Just (["B", "C"], AssignmentState 2 (Info.Pos 1 3) "" [])

    it "matches multiple nested children" $
      runAssignment headF
        (symbol Red *> children (many (symbol Green *> children (symbol Blue *> source))))
        (makeState "12" [ node Red 0 2 [ node Green 0 1 [ node Blue 0 1 [] ]
                                       , node Green 1 2 [ node Blue 1 2 [] ] ] ])
      `shouldBe`
        Result Nothing (Just (["1", "2"], AssignmentState 2 (Info.Pos 1 3) "" []))

  describe "runAssignment" $ do
    it "drops anonymous nodes before matching symbols" $
      runAssignment headF red (makeState "magenta red" [node Magenta 0 7 [], node Red 8 11 []]) `shouldBe` Result Nothing (Just (Out "red", AssignmentState 11 (Info.Pos 1 12) "" []))

    it "does not drop anonymous nodes after matching" $
      runAssignment headF red (makeState "red magenta" [node Red 0 3 [], node Magenta 4 11 []]) `shouldBe` Result Nothing (Just (Out "red", AssignmentState 3 (Info.Pos 1 4) " magenta" [node Magenta 4 11 []]))

    it "does not drop anonymous nodes when requested" $
      runAssignment headF ((,) <$> magenta <*> red) (makeState "magenta red" [node Magenta 0 7 [], node Red 8 11 []]) `shouldBe` Result Nothing (Just ((Out "magenta", Out "red"), AssignmentState 11 (Info.Pos 1 12) "" []))

node :: symbol -> Int -> Int -> [AST symbol] -> AST symbol
node symbol start end children = cofree $ (Just symbol :. Range start end :. Info.Span (Info.Pos 1 (succ start)) (Info.Pos 1 (succ end)) :. Nil) :< children

data Grammar = Red | Green | Blue | Magenta
  deriving (Enum, Eq, Show)

instance Symbol Grammar where
  symbolType Magenta = Anonymous
  symbolType _ = Regular

data Out = Out ByteString
  deriving (Eq, Show)

red :: Assignment (AST Grammar) Grammar Out
red = Out <$ symbol Red <*> source

green :: Assignment (AST Grammar) Grammar Out
green = Out <$ symbol Green <*> source

blue :: Assignment (AST Grammar) Grammar Out
blue = Out <$ symbol Blue <*> source

magenta :: Assignment (AST Grammar) Grammar Out
magenta = Out <$ symbol Magenta <*> source

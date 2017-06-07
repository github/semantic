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
      runAssignment ((,) <$> red <*> red) (makeState "helloworld" [node Red 0 5 [], node Red 5 10 []]) `shouldBe` Result Nothing (Just (AssignmentState 10 (Info.SourcePos 1 11) "" [], (Out "hello", Out "world")))

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      runAssignment (green <|> red) (makeState "hello" [node Red 0 5 []]) `shouldBe` Result Nothing (Just (AssignmentState 5 (Info.SourcePos 1 6) "" [], Out "hello"))

    it "matches repetitions" $
      let s = "colourless green ideas sleep furiously"
          w = words s
          (_, nodes) = foldl (\ (i, prev) word -> (i + B.length word + 1, prev <> [node Red i (i + B.length word) []])) (0, []) w in
      resultValue (runAssignment (many red) (makeState (Source s) nodes)) `shouldBe` Just (AssignmentState (B.length s) (Info.SourcePos 1 (succ (B.length s))) "" [], Out <$> w)

    it "matches one-or-more repetitions against one or more input nodes" $
      resultValue (runAssignment (some red) (makeState "hello" [node Red 0 5 []])) `shouldBe` Just (AssignmentState 5 (Info.SourcePos 1 6) "" [], [Out "hello"])

  describe "symbol" $ do
    it "matches nodes with the same symbol" $
      snd <$> runAssignment red (makeState "hello" [node Red 0 5 []]) `shouldBe` Result Nothing (Just (Out "hello"))

    it "does not advance past the current node" $
      let initialState = makeState "hi" [ node Red 0 2 [] ] in
      fst <$> runAssignment (symbol Red) initialState `shouldBe` Result Nothing (Just initialState)

  describe "source" $ do
    it "produces the node’s source" $
      assign source "hi" (node Red 0 2 []) `shouldBe` Result Nothing (Just "hi")

    it "advances past the current node" $
      fst <$> runAssignment source (makeState "hi" [ node Red 0 2 [] ]) `shouldBe` Result Nothing (Just (AssignmentState 2 (Info.SourcePos 1 3) "" []))

  describe "children" $ do
    it "advances past the current node" $
      fst <$> runAssignment (children (pure (Out ""))) (makeState "a" [node Red 0 1 []]) `shouldBe` Result Nothing (Just (AssignmentState 1 (Info.SourcePos 1 2) "" []))

    it "matches if its subrule matches" $
      () <$ runAssignment (children red) (makeState "a" [node Blue 0 1 [node Red 0 1 []]]) `shouldBe` Result Nothing (Just ())

    it "does not match if its subrule does not match" $
      (runAssignment (children red) (makeState "a" [node Blue 0 1 [node Green 0 1 []]])) `shouldBe` Result (Just (Error (Info.SourcePos 1 1) (UnexpectedSymbol [Red] Green))) Nothing

    it "matches nested children" $
      runAssignment
        (symbol Red *> children (symbol Green *> children (symbol Blue *> source)))
        (makeState "1" [ node Red 0 1 [ node Green 0 1 [ node Blue 0 1 [] ] ] ])
      `shouldBe`
        Result Nothing (Just (AssignmentState 1 (Info.SourcePos 1 2) "" [], "1"))

    it "continues after children" $
      resultValue (runAssignment
        (many (symbol Red *> children (symbol Green *> source)
           <|> symbol Blue *> source))
        (makeState "BC" [ node Red 0 1 [ node Green 0 1 [] ]
                        , node Blue 1 2 [] ]))
      `shouldBe`
        Just (AssignmentState 2 (Info.SourcePos 1 3) "" [], ["B", "C"])

    it "matches multiple nested children" $
      runAssignment
        (symbol Red *> children (many (symbol Green *> children (symbol Blue *> source))))
        (makeState "12" [ node Red 0 2 [ node Green 0 1 [ node Blue 0 1 [] ]
                                       , node Green 1 2 [ node Blue 1 2 [] ] ] ])
      `shouldBe`
        Result Nothing (Just (AssignmentState 2 (Info.SourcePos 1 3) "" [], ["1", "2"]))

  describe "runAssignment" $ do
    it "drops anonymous nodes before matching symbols" $
      runAssignment red (makeState "magenta red" [node Magenta 0 7 [], node Red 8 11 []]) `shouldBe` Result Nothing (Just (AssignmentState 11 (Info.SourcePos 1 12) "" [], Out "red"))

    it "does not drop anonymous nodes after matching" $
      runAssignment red (makeState "red magenta" [node Red 0 3 [], node Magenta 4 11 []]) `shouldBe` Result Nothing (Just (AssignmentState 3 (Info.SourcePos 1 4) " magenta" [node Magenta 4 11 []], Out "red"))

    it "does not drop anonymous nodes when requested" $
      runAssignment ((,) <$> magenta <*> red) (makeState "magenta red" [node Magenta 0 7 [], node Red 8 11 []]) `shouldBe` Result Nothing (Just (AssignmentState 11 (Info.SourcePos 1 12) "" [], (Out "magenta", Out "red")))

node :: symbol -> Int -> Int -> [AST symbol] -> AST symbol
node symbol start end children = cofree $ (Just symbol :. Range start end :. Info.SourceSpan (Info.SourcePos 1 (succ start)) (Info.SourcePos 1 (succ end)) :. Nil) :< children

data Grammar = Red | Green | Blue | Magenta
  deriving (Enum, Eq, Show)

instance Symbol Grammar where
  symbolType Magenta = Anonymous
  symbolType _ = Regular

data Out = Out ByteString
  deriving (Eq, Show)

red :: Assignment Grammar Out
red = Out <$ symbol Red <*> source

green :: Assignment Grammar Out
green = Out <$ symbol Green <*> source

blue :: Assignment Grammar Out
blue = Out <$ symbol Blue <*> source

magenta :: Assignment Grammar Out
magenta = Out <$ symbol Magenta <*> source

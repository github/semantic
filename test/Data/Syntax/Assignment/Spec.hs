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
  describe "Applicative" $ do
    it "matches in sequence" $
      runAssignment ((,) <$> red <*> red) (AssignmentState 0 (Source "helloworld") [Rose (rec Red 0 5) [], Rose (rec Red 5 10) []]) `shouldBe` Result (AssignmentState 10 (Source "") [], (Out "hello", Out "world"))

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      runAssignment (green <|> red) (AssignmentState 0 (Source "hello") [Rose (rec Red 0 5) []]) `shouldBe` Result (AssignmentState 5 (Source "") [], Out "hello")

    it "matches repetitions" $
      let s = "colourless green ideas sleep furiously"
          w = words s
          (_, nodes) = foldl (\ (i, prev) word -> (i + B.length word + 1, prev <> [Rose (rec Red i (i + B.length word)) []])) (0, []) w in
      runAssignment (many red) (AssignmentState 0 (Source s) nodes) `shouldBe` Result (AssignmentState (B.length s) (Source "") [], Out <$> w)

    it "matches one-or-more repetitions against one or more input nodes" $
      runAssignment (some red) (AssignmentState 0 (Source "hello") [Rose (rec Red 0 5) []]) `shouldBe` Result (AssignmentState 5 (Source "") [], [Out "hello"])

  describe "symbol" $ do
    it "matches nodes with the same symbol" $
      snd <$> runAssignment red (AssignmentState 0 (Source "hello") [Rose (rec Red 0 5) []]) `shouldBe` Result (Out "hello")

    it "does not advance past the current node" $
      fst <$> runAssignment (symbol Red) (AssignmentState 0 (Source "hi") [ Rose (rec Red 0 2) [] ]) `shouldBe` Result (AssignmentState 0 (Source "hi") [ Rose (rec Red 0 2) [] ])

  describe "source" $ do
    it "produces the node’s source" $
      assignAll source (Source "hi") [ Rose (rec Red 0 2) [] ] `shouldBe` Result "hi"

    it "advances past the current node" $
      fst <$> runAssignment source (AssignmentState 0 (Source "hi") [ Rose (rec Red 0 2) [] ]) `shouldBe` Result (AssignmentState 2 (Source "") [])

  describe "children" $ do
    it "advances past the current node" $
      fst <$> runAssignment (children (pure (Out ""))) (AssignmentState 0 (Source "a") [Rose (rec Red 0 1) []]) `shouldBe` Result (AssignmentState 1 (Source "") [])

    it "matches if its subrule matches" $
      () <$ runAssignment (children red) (AssignmentState 0 (Source "a") [Rose (rec Blue 0 1) [Rose (rec Red 0 1) []]]) `shouldBe` Result ()

    it "does not match if its subrule does not match" $
      let errors r = case r of { Result _ -> [] ; Error e -> e } in
      Prologue.length (errors (runAssignment (children red) (AssignmentState 0 (Source "a") [Rose (rec Blue 0 1) [Rose (rec Green 0 1) []]]))) `shouldBe` 1

    it "matches nested children" $ do
      runAssignment
        (symbol Red *> children (symbol Green *> children (symbol Blue *> source)))
        (AssignmentState 0 (Source "1") [ Rose (rec Red 0 1) [ Rose (rec Green 0 1) [ Rose (rec Blue 0 1) [] ] ] ])
      `shouldBe`
        Result (AssignmentState 1 (Source "") [], "1")

    it "continues after children" $ do
      runAssignment
        (many (symbol Red *> children (symbol Green *> source)
           <|> symbol Blue *> source))
        (AssignmentState 0 (Source "BC") [ Rose (rec Red 0 1) [ Rose (rec Green 0 1) [] ]
                                         , Rose (rec Blue 1 2) [] ])
      `shouldBe`
        Result (AssignmentState 2 (Source "") [], ["B", "C"])

    it "matches multiple nested children" $ do
      runAssignment
        (symbol Red *> children (many (symbol Green *> children (symbol Blue *> source))))
        (AssignmentState 0 (Source "12") [ Rose (rec Red 0 2) [ Rose (rec Green 0 1) [ Rose (rec Blue 0 1) [] ]
                                                              , Rose (rec Green 1 2) [ Rose (rec Blue 1 2) [] ] ] ])
      `shouldBe`
        Result (AssignmentState 2 (Source "") [], ["1", "2"])

rec :: symbol -> Int -> Int -> Record '[symbol, Range, SourceSpan]
rec symbol start end = symbol :. Range start end :. Info.SourceSpan (Info.SourcePos 0 0) (Info.SourcePos 0 0) :. Nil

data Grammar = Red | Green | Blue
  deriving (Eq, Show)

instance Symbol Grammar where
  symbolType _ = Regular

data Out = Out ByteString
  deriving (Eq, Show)

red :: Assignment (Node Grammar) Out
red = Out <$ symbol Red <*> source

green :: Assignment (Node Grammar) Out
green = Out <$ symbol Green <*> source

blue :: Assignment (Node Grammar) Out
blue = Out <$ symbol Blue <*> source

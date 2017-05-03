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
      runAssignment ((,) <$> red <*> red) (makeState "helloworld" [Rose (rec Red 0 5) [], Rose (rec Red 5 10) []]) `shouldBe` Result [] (Just (AssignmentState 10 (Info.SourcePos 1 11) (Source "") [], (Out "hello", Out "world")))

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      runAssignment (green <|> red) (makeState "hello" [Rose (rec Red 0 5) []]) `shouldBe` Result [] (Just (AssignmentState 5 (Info.SourcePos 1 6) (Source "") [], Out "hello"))

    it "matches repetitions" $
      let s = "colourless green ideas sleep furiously"
          w = words s
          (_, nodes) = foldl (\ (i, prev) word -> (i + B.length word + 1, prev <> [Rose (rec Red i (i + B.length word)) []])) (0, []) w in
      resultValue (runAssignment (many red) (makeState (Source s) nodes)) `shouldBe` Just (AssignmentState (B.length s) (Info.SourcePos 1 (succ (B.length s))) (Source "") [], Out <$> w)

    it "matches one-or-more repetitions against one or more input nodes" $
      resultValue (runAssignment (some red) (makeState "hello" [Rose (rec Red 0 5) []])) `shouldBe` Just (AssignmentState 5 (Info.SourcePos 1 6) (Source "") [], [Out "hello"])

  describe "symbol" $ do
    it "matches nodes with the same symbol" $
      snd <$> runAssignment red (makeState "hello" [Rose (rec Red 0 5) []]) `shouldBe` Result [] (Just (Out "hello"))

    it "does not advance past the current node" $
      fst <$> runAssignment (symbol Red) (makeState "hi" [ Rose (rec Red 0 2) [] ]) `shouldBe` Result [] (Just (AssignmentState 0 (Info.SourcePos 1 1) (Source "hi") [ Rose (rec Red 0 2) [] ]))

  describe "source" $ do
    it "produces the node’s source" $
      assign source (Source "hi") (Rose (rec Red 0 2) []) `shouldBe` Result [] (Just "hi")

    it "advances past the current node" $
      fst <$> runAssignment source (makeState "hi" [ Rose (rec Red 0 2) [] ]) `shouldBe` Result [] (Just (AssignmentState 2 (Info.SourcePos 1 3) (Source "") []))

  describe "children" $ do
    it "advances past the current node" $
      fst <$> runAssignment (children (pure (Out ""))) (makeState "a" [Rose (rec Red 0 1) []]) `shouldBe` Result [] (Just (AssignmentState 1 (Info.SourcePos 1 2) (Source "") []))

    it "matches if its subrule matches" $
      () <$ runAssignment (children red) (makeState "a" [Rose (rec Blue 0 1) [Rose (rec Red 0 1) []]]) `shouldBe` Result [] (Just ())

    it "does not match if its subrule does not match" $
      (runAssignment (children red) (makeState "a" [Rose (rec Blue 0 1) [Rose (rec Green 0 1) []]])) `shouldBe` Result [ Error (Info.SourcePos 1 1) [Red] (Just Green) ] Nothing

    it "matches nested children" $ do
      runAssignment
        (symbol Red *> children (symbol Green *> children (symbol Blue *> source)))
        (makeState "1" [ Rose (rec Red 0 1) [ Rose (rec Green 0 1) [ Rose (rec Blue 0 1) [] ] ] ])
      `shouldBe`
        Result [] (Just (AssignmentState 1 (Info.SourcePos 1 2) (Source "") [], "1"))

    it "continues after children" $ do
      resultValue (runAssignment
        (many (symbol Red *> children (symbol Green *> source)
           <|> symbol Blue *> source))
        (makeState "BC" [ Rose (rec Red 0 1) [ Rose (rec Green 0 1) [] ]
                        , Rose (rec Blue 1 2) [] ]))
      `shouldBe`
        Just (AssignmentState 2 (Info.SourcePos 1 3) (Source "") [], ["B", "C"])

    it "matches multiple nested children" $ do
      runAssignment
        (symbol Red *> children (many (symbol Green *> children (symbol Blue *> source))))
        (makeState "12" [ Rose (rec Red 0 2) [ Rose (rec Green 0 1) [ Rose (rec Blue 0 1) [] ]
                                             , Rose (rec Green 1 2) [ Rose (rec Blue 1 2) [] ] ] ])
      `shouldBe`
        Result [] (Just (AssignmentState 2 (Info.SourcePos 1 3) (Source "") [], ["1", "2"]))

rec :: symbol -> Int -> Int -> Record '[symbol, Range, SourceSpan]
rec symbol start end = symbol :. Range start end :. Info.SourceSpan (Info.SourcePos 1 (succ start)) (Info.SourcePos 1 (succ end)) :. Nil

data Grammar = Red | Green | Blue
  deriving (Enum, Eq, Show)

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

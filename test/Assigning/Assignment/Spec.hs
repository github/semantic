{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Assigning.Assignment.Spec (spec) where

import Assigning.Assignment
import Data.AST
import Data.Bifunctor (first)
import Data.Ix
import Data.Semigroup ((<>))
import Data.Term
import Data.Text as T (Text, length, words)
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack (getCallStack)
import Prelude hiding (words)
import Source.Range
import Source.Source
import Source.Span
import Test.Hspec
import TreeSitter.Language (Symbol (..), SymbolType (..))

spec :: Spec
spec = do
  describe "Applicative" $
    it "matches in sequence" $
      fst <$> runAssignment "helloworld" ((,) <$> red <*> red) (makeState [node Red 0 5 [], node Red 5 10 []])
      `shouldBe`
        Right (Out "hello", Out "world")

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      fst <$> runAssignment "hello" (green <|> red) (makeState [node Red 0 5 []])
      `shouldBe`
        Right (Out "hello")

    it "matches repetitions" $
      let s = "colourless green ideas sleep furiously"
          w = words s
          (_, nodes) = foldl (\ (i, prev) word -> (i + T.length word + 1, prev <> [node Red i (i + T.length word) []])) (0, []) w in
      fst <$> runAssignment (fromUTF8 (encodeUtf8 s)) (many red) (makeState nodes)
      `shouldBe`
        Right (Out <$> w)

    it "matches one-or-more repetitions against one or more input nodes" $
      fst <$> runAssignment "hello" (some red) (makeState [node Red 0 5 []])
      `shouldBe`
        Right [Out "hello"]

    describe "distributing through overlapping committed choices" $ do

      it "matches the left alternative" $
        fst <$> runAssignment "(red (green))" (symbol Red *> children green <|> symbol Red *> children blue) (makeState [node Red 0 13 [node Green 5 12 []]])
        `shouldBe`
        Right (Out "(green)")

      it "matches the right alternative" $
        fst <$> runAssignment "(red (blue))" (symbol Red *> children green <|> symbol Red *> children blue) (makeState [node Red 0 12 [node Blue 5 11 []]])
        `shouldBe`
        Right (Out "(blue)")

      it "matches the left alternatives" $
        fst <$> runAssignment "magenta green green" (symbol Magenta *> many green <|> symbol Magenta *> many blue) (makeState [node Magenta 0 7 [], node Green 8 13 [], node Green 14 19 []])
        `shouldBe`
        Right [Out "green", Out "green"]

      it "matches the empty list" $
        fst <$> runAssignment "magenta" (symbol Magenta *> (Left <$> many green) <|> symbol Magenta *> (Right <$> many blue)) (makeState [node Magenta 0 7 []])
        `shouldBe`
        Right (Left [])

      it "drops anonymous nodes & matches the left alternative" $
        fst <$> runAssignment "magenta green" (symbol Magenta *> green <|> symbol Magenta *> blue) (makeState [node Magenta 0 7 [], node Green 8 13 []])
        `shouldBe`
        Right (Out "green")

      it "drops anonymous nodes & matches the right alternative" $
        fst <$> runAssignment "magenta blue" (symbol Magenta *> green <|> symbol Magenta *> blue) (makeState [node Magenta 0 7 [], node Blue 8 12 []])
        `shouldBe`
        Right (Out "blue")

    it "alternates repetitions, matching the left alternative" $
      fst <$> runAssignment "green green" (many green <|> many blue) (makeState [node Green 0 5 [], node Green 6 11 []])
      `shouldBe`
      Right [Out "green", Out "green"]

    it "alternates repetitions, matching at the end of input" $
      fst <$> runAssignment "" (many green <|> many blue) (makeState [])
      `shouldBe`
      Right []

    it "distributes through children rules" $
      fst <$> runAssignment "(red (blue))" (children green <|> children blue) (makeState [node Red 0 12 [node Blue 5 11 []]])
      `shouldBe`
      Right (Out "(blue)")

    it "matches rules to the left of pure" $
      fst <$> runAssignment "green" ((green <|> pure (Out "other") <|> blue) <* many source) (makeState [node Green 0 5 []])
      `shouldBe`
      Right (Out "green")

    it "matches pure instead of rules to its right" $
      fst <$> runAssignment "blue" ((green <|> pure (Out "other") <|> blue) <* many source) (makeState [node Blue 0 4 []])
      `shouldBe`
      Right (Out "other")

    it "matches other nodes with pure" $
      fst <$> runAssignment "red" ((green <|> pure (Out "other") <|> blue) <* many source) (makeState [node Red 0 3 []])
      `shouldBe`
      Right (Out "other")

    it "matches at end with pure" $
      fst <$> runAssignment "red" ((green <|> pure (Out "other") <|> blue) <* many source) (makeState [])
      `shouldBe`
      Right (Out "other")

  describe "symbol" $ do
    it "matches nodes with the same symbol" $
      fst <$> runAssignment "hello" red (makeState [node Red 0 5 []]) `shouldBe` Right (Out "hello")

    it "does not advance past the current node" $
      runAssignment "hi" (symbol Red) (makeState [ node Red 0 2 [] ]) `shouldBe` Left (Error (Span (Pos 1 1) (Pos 1 3)) [] (Just (Right Red)) [])

  describe "without catchError" $ do
    it "assignment returns unexpected symbol error" $
      runAssignment "A"
        red
        (makeState [node Green 0 1 []])
        `shouldBe`
          Left (Error (Span (Pos 1 1) (Pos 1 2)) [Right Red] (Just (Right Green)) [])

    it "assignment returns unexpected end of input" $
      runAssignment "A"
        (symbol Green *> children (some red))
        (makeState [node Green 0 1 []])
        `shouldBe`
          Left (Error (Span (Pos 1 1) (Pos 1 1)) [Right Red] Nothing [])

  describe "eof" $ do
    it "matches at the end of branches" $
      fst <$> runAssignment "" eof (makeState [] :: State [] Grammar) `shouldBe` Right ()

    it "matches before anonymous nodes at the end of branches" $
      fst <$> runAssignment "magenta" eof (makeState [ node Magenta 0 7 [] ] :: State [] Grammar) `shouldBe` Right ()

  describe "catchError" $ do
    it "catches failed committed choices" $
      fst <$> runAssignment "A"
        ((symbol Green *> children red) `catchError` \ _ -> OutError <$ location <*> source)
        (makeState [node Green 0 1 []])
        `shouldBe`
          Right (OutError "A")

    it "doesn’t catch uncommitted choices" $
      fst <$> runAssignment "A"
        (red `catchError` \ _ -> OutError <$ location <*> source)
        (makeState [node Green 0 1 []])
        `shouldBe`
          Left (Error (Span (Pos 1 1) (Pos 1 2)) [Right Red] (Just (Right Green)) [])

    it "doesn’t catch unexpected end of branch" $
      fst <$> runAssignment ""
        (red `catchError` \ _ -> OutError <$ location <*> source)
        (makeState [])
        `shouldBe`
          Left (Error (Span (Pos 1 1) (Pos 1 1)) [Right Red] Nothing [])

    it "doesn’t catch exhaustiveness errors" $
      fst <$> runAssignment "AA"
        (red `catchError` \ _ -> OutError <$ location <*> source)
        (makeState [node Red 0 1 [], node Red 1 2 []])
        `shouldBe`
          Left (Error (Span (Pos 1 2) (Pos 1 3)) [] (Just (Right Red)) [])

    it "can error inside the handler" $
      runAssignment "A"
        (symbol Green *> children red `catchError` const blue)
        (makeState [node Green 0 1 []])
        `shouldBe`
          Left (Error (Span (Pos 1 1) (Pos 1 1)) [Right Red] Nothing [])

  describe "many" $ do
    it "takes ones and only one zero width repetition" $
      fst <$> runAssignment "PGG"
        (symbol Palette *> children ( many (green <|> pure (Out "always")) ))
        (makeState [node Palette 0 1 [node Green 1 2 [], node Green 2 3 []]])
        `shouldBe`
          Right [Out "G", Out "G", Out "always"]

  describe "source" $ do
    it "produces the node’s source" $
      assign "hi" source (node Red 0 2 []) `shouldBe` Right ("hi")

    it "advances past the current node" $
      snd <$> runAssignment "hi" source (makeState [ node Red 0 2 [] ])
      `shouldBe`
        Right (State 2 (Pos 1 3) [] [] [])

  describe "children" $ do
    it "advances past the current node" $
      snd <$> runAssignment "a" (children (pure (Out ""))) (makeState [node Red 0 1 []])
      `shouldBe`
        Right (State 1 (Pos 1 2) [] [] [])

    it "matches if its subrule matches" $
      () <$ runAssignment "a" (children red) (makeState [node Blue 0 1 [node Red 0 1 []]])
      `shouldBe`
        Right ()

    it "does not match if its subrule does not match" $
      runAssignment "a" (children red) (makeState [node Blue 0 1 [node Green 0 1 []]])
      `shouldBe`
        Left (Error (Span (Pos 1 1) (Pos 1 2)) [Right Red] (Just (Right Green)) [])

    it "matches nested children" $
      fst <$> runAssignment "1"
        (symbol Red *> children (symbol Green *> children (symbol Blue *> source)))
        (makeState [ node Red 0 1 [ node Green 0 1 [ node Blue 0 1 [] ] ] ])
      `shouldBe`
        Right "1"

    it "continues after children" $
      fst <$> runAssignment "BC"
        (many (symbol Red *> children (symbol Green *> source)
           <|> symbol Blue *> source))
        (makeState [ node Red 0 1 [ node Green 0 1 [] ]
                   , node Blue 1 2 [] ])
      `shouldBe`
        Right ["B", "C"]

    it "matches multiple nested children" $
      fst <$> runAssignment "12"
        (symbol Red *> children (many (symbol Green *> children (symbol Blue *> source))))
        (makeState [ node Red 0 2 [ node Green 0 1 [ node Blue 0 1 [] ]
                                  , node Green 1 2 [ node Blue 1 2 [] ] ] ])
      `shouldBe`
        Right ["1", "2"]

  describe "runAssignment" $ do
    it "drops anonymous nodes before matching symbols" $
      fst <$> runAssignment "magenta red" red (makeState [node Magenta 0 7 [], node Red 8 11 []])
      `shouldBe`
        Right (Out "red")

    it "drops anonymous nodes after matching to ensure exhaustiveness" $
      stateNodes . snd <$> runAssignment "red magenta" red (makeState [node Red 0 3 [], node Magenta 4 11 []])
      `shouldBe`
        Right []

    it "does not drop anonymous nodes when requested" $
      fst <$> runAssignment "magenta red" ((,) <$> magenta <*> red) (makeState [node Magenta 0 7 [], node Red 8 11 []])
      `shouldBe`
        Right (Out "magenta", Out "red")

    it "produces errors with callstacks pointing at the failing assignment" $
      first (fmap fst . getCallStack . errorCallStack) (runAssignment "blue" red (makeState [node Blue 0 4 []]))
      `shouldBe`
      Left [ "symbol" ]

node :: symbol -> Int -> Int -> [AST [] symbol] -> AST [] symbol
node symbol start end children = Term (Node symbol (Loc (Range start end) (Span (Pos 1 (succ start)) (Pos 1 (succ end)))) `In` children)

data Grammar = Palette | Red | Green | Blue | Magenta
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

instance Symbol Grammar where
  symbolType Magenta = Anonymous
  symbolType _       = Regular

data Out = Out T.Text | OutError T.Text
  deriving (Eq, Show)

red :: HasCallStack => Assignment [] Grammar Out
red = Out <$ symbol Red <*> source

green :: HasCallStack => Assignment [] Grammar Out
green = Out <$ symbol Green <*> source

blue :: HasCallStack => Assignment [] Grammar Out
blue = Out <$ symbol Blue <*> source

magenta :: HasCallStack => Assignment [] Grammar Out
magenta = Out <$ symbol Magenta <*> source

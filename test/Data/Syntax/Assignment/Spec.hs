{-# LANGUAGE DataKinds #-}
module Data.Syntax.Assignment.Spec where

import Control.Comonad.Cofree (Cofree(..))
import Control.Comonad.Trans.Cofree (headF)
import Data.Amb
import Data.Bifunctor (first)
import Data.ByteString.Char8 as B (ByteString, length, words)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import Data.Source
import Data.Syntax.Assignment
import GHC.Stack (getCallStack)
import Info
import Prelude hiding (words)
import Test.Hspec
import Text.Parser.TreeSitter.Language (Symbol(..), SymbolType(..))

spec :: Spec
spec = do
  describe "Applicative" $
    it "matches in sequence" $
      fst <$> runAssignment headF "helloworld" ((,) <$> red <*> red) (makeState [node Red 0 5 [], node Red 5 10 []])
      `shouldBe`
        Some ((Out "hello", Out "world") :| [])

  describe "Alternative" $ do
    it "attempts multiple alternatives" $
      fst <$> runAssignment headF "hello" (green <|> red) (makeState [node Red 0 5 []])
      `shouldBe`
        Some ((Out "hello") :| [])

    it "matches repetitions" $
      let s = "colourless green ideas sleep furiously"
          w = words s
          (_, nodes) = foldl (\ (i, prev) word -> (i + B.length word + 1, prev <> [node Red i (i + B.length word) []])) (0, []) w in
      fst <$> runAssignment headF (fromBytes s) (many red) (makeState nodes)
      `shouldBe`
        Some ((Out <$> w) :| [])

    it "matches one-or-more repetitions against one or more input nodes" $
      fst <$> runAssignment headF "hello" (some red) (makeState [node Red 0 5 []])
      `shouldBe`
        Some ([Out "hello"] :| [])

    it "distributes through overlapping committed choices, matching the left alternative" $
      fst <$> runAssignment headF "(red (green))" (symbol Red *> children green <|> symbol Red *> children blue) (makeState [node Red 0 13 [node Green 5 12 []]])
      `shouldBe`
      Some (Out "(green)" :| [])

    it "distributes through overlapping committed choices, matching the right alternative" $
      fst <$> runAssignment headF "(red (blue))" (symbol Red *> children green <|> symbol Red *> children blue) (makeState [node Red 0 12 [node Blue 5 11 []]])
      `shouldBe`
      Some (Out "(blue)" :| [])

    it "distributes through overlapping committed choices, matching the left alternatives" $
      fst <$> runAssignment headF "magenta green green" (symbol Magenta *> many green <|> symbol Magenta *> many blue) (makeState [node Magenta 0 7 [], node Green 8 13 [], node Green 14 19 []])
      `shouldBe`
      Some ([Out "green", Out "green"] :| [])

    it "distributes through overlapping committed choices, matching the right alternatives" $
      fst <$> runAssignment headF "magenta blue blue" (symbol Magenta *> many green <|> symbol Magenta *> many blue) (makeState [node Magenta 0 7 [], node Blue 8 12 [], node Blue 13 17 []])
      `shouldBe`
      Some ([Out "blue", Out "blue"] :| [])

    it "distributes through overlapping committed choices, matching the empty list" $
      fst <$> runAssignment headF "magenta" (symbol Magenta *> (Left <$> many green) <|> symbol Magenta *> (Right <$> many blue)) (makeState [node Magenta 0 7 []])
      `shouldBe`
      Some (Left [] :| [])

    it "distributes through overlapping committed choices, dropping anonymous nodes & matching the left alternative" $
      fst <$> runAssignment headF "magenta green" (symbol Magenta *> green <|> symbol Magenta *> blue) (makeState [node Magenta 0 7 [], node Green 8 13 []])
      `shouldBe`
      Some (Out "green" :| [])

    it "distributes through overlapping committed choices, dropping anonymous nodes & matching the right alternative" $
      fst <$> runAssignment headF "magenta blue" (symbol Magenta *> green <|> symbol Magenta *> blue) (makeState [node Magenta 0 7 [], node Blue 8 12 []])
      `shouldBe`
      Some (Out "blue" :| [])

    it "alternates repetitions, matching the left alternative" $
      fst <$> runAssignment headF "green green" (many green <|> many blue) (makeState [node Green 0 5 [], node Green 6 11 []])
      `shouldBe`
      Some ([Out "green", Out "green"] :| [])

    it "alternates repetitions, matching the right alternative" $
      fst <$> runAssignment headF "blue blue" (many green <|> many blue) (makeState [node Blue 0 4 [], node Blue 5 9 []])
      `shouldBe`
      Some ([Out "blue", Out "blue"] :| [])

    it "alternates repetitions, matching at the end of input" $
      fst <$> runAssignment headF "" (many green <|> many blue) (makeState [])
      `shouldBe`
      Some ([] :| [])

    it "distributes through children rules" $
      fst <$> runAssignment headF "(red (blue))" (children (many green) <|> children (many blue)) (makeState [node Red 0 12 [node Blue 5 11 []]])
      `shouldBe`
      Some ([Out "(blue)"] :| [])

    it "matches rules to the left of pure" $
      fst <$> runAssignment headF "green" ((green <|> pure (Out "other") <|> blue) <* many source) (makeState [node Green 0 5 []])
      `shouldBe`
      Some (Out "green" :| [])

    it "matches rules to the right of pure" $
      fst <$> runAssignment headF "blue" ((green <|> pure (Out "other") <|> blue) <* many source) (makeState [node Blue 0 4 []])
      `shouldBe`
      Some (Out "blue" :| [])

    it "matches other nodes with pure" $
      fst <$> runAssignment headF "red" ((green <|> pure (Out "other") <|> blue) <* many source) (makeState [node Red 0 3 []])
      `shouldBe`
      Some (Out "other" :| [])

    it "matches at end with pure" $
      fst <$> runAssignment headF "red" ((green <|> pure (Out "other") <|> blue) <* many source) (makeState [])
      `shouldBe`
      Some (Out "other" :| [])

  describe "symbol" $ do
    it "matches nodes with the same symbol" $
      fst <$> runAssignment headF "hello" red (makeState [node Red 0 5 []]) `shouldBe` Some (Out "hello" :| [])

    it "does not advance past the current node" $
      runAssignment headF "hi" (symbol Red) (makeState [ node Red 0 2 [] ]) `shouldBe` None (Error (Info.Pos 1 1) [] (Just Red))

  describe "without catchError" $ do
    it "assignment returns unexpected symbol error" $
      runAssignment headF "A"
        red
        (makeState [node Green 0 1 []])
        `shouldBe`
          None (Error (Info.Pos 1 1) [Red] (Just Green))

    it "assignment returns unexpected end of input" $
      runAssignment headF "A"
        (symbol Green *> children (some red))
        (makeState [node Green 0 1 []])
        `shouldBe`
          None (Error (Info.Pos 1 1) [Red] Nothing)

  describe "catchError" $ do
    it "handler that always matches" $
      fst <$> runAssignment headF "A"
        (red `catchError` (\ _ -> OutError <$ location <*> source))
        (makeState [node Green 0 1 []])
        `shouldBe`
          Some (OutError "A" :| [])

    it "handler that matches" $
      fst <$> runAssignment headF "A"
        (red `catchError` const green)
        (makeState [node Green 0 1 []])
        `shouldBe`
          Some (Out "A" :| [])

    it "handler that doesn't match produces error" $
      runAssignment headF "A"
        (red `catchError` const blue)
        (makeState [node Green 0 1 []])
        `shouldBe`
          None (Error (Info.Pos 1 1) [Red] (Just Green))

    describe "in many" $ do
      it "handler that always matches" $
        fst <$> runAssignment headF "PG"
          (symbol Palette *> children (
            many (red `catchError` (\ _ -> OutError <$ location <*> source))
          ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            Some ([OutError "G"] :| [])

      it "handler that matches" $
        fst <$> runAssignment headF "PG"
          (symbol Palette *> children ( many (red `catchError` const green) ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            Some ([Out "G"] :| [])

      it "handler that doesn't match produces error" $
        runAssignment headF "PG"
          (symbol Palette *> children ( many (red `catchError` const blue) ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            None (Error (Info.Pos 1 2) [] (Just Green))

      it "handler that always matches with apply consumes and then errors" $
        runAssignment headF "PG"
          (symbol Palette *> children (
            (,) <$> many (red `catchError` (\ _ -> OutError <$ location <*> source)) <*> green
          ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            None (Error (Info.Pos 1 3) [Green] Nothing)

      it "handler that doesn't match with apply" $
        fst <$> runAssignment headF "PG"
          (symbol Palette *> children (
            (,) <$> many (red `catchError` const blue) <*> green
          ))
          (makeState [node Palette 0 1 [node Green 1 2 []]])
          `shouldBe`
            Some (([], Out "G") :| [])

  describe "many" $ do
    it "takes ones and only one zero width repetition" $
      fst <$> runAssignment headF "PGG"
        (symbol Palette *> children ( many (green <|> pure (Out "always")) ))
        (makeState [node Palette 0 1 [node Green 1 2 [], node Green 2 3 []]])
        `shouldBe`
          Some ([Out "G", Out "G", Out "always"] :| [])

  describe "source" $ do
    it "produces the node’s source" $
      assignBy headF "hi" source (node Red 0 2 []) `shouldBe` Some ("hi" :| [])

    it "advances past the current node" $
      snd <$> runAssignment headF "hi" source (makeState [ node Red 0 2 [] ])
      `shouldBe`
        Some ((State 2 (Info.Pos 1 3) 0 []) :| [])

  describe "children" $ do
    it "advances past the current node" $
      snd <$> runAssignment headF "a" (children (pure (Out ""))) (makeState [node Red 0 1 []])
      `shouldBe`
        Some (State 1 (Info.Pos 1 2) 0 [] :| [])

    it "matches if its subrule matches" $
      () <$ runAssignment headF "a" (children red) (makeState [node Blue 0 1 [node Red 0 1 []]])
      `shouldBe`
        Some (() :| [])

    it "does not match if its subrule does not match" $
      runAssignment headF "a" (children red) (makeState [node Blue 0 1 [node Green 0 1 []]])
      `shouldBe`
        None (Error (Info.Pos 1 1) [Red] (Just Green))

    it "matches nested children" $
      fst <$> runAssignment headF "1"
        (symbol Red *> children (symbol Green *> children (symbol Blue *> source)))
        (makeState [ node Red 0 1 [ node Green 0 1 [ node Blue 0 1 [] ] ] ])
      `shouldBe`
        Some ("1" :| [])

    it "continues after children" $
      fst <$> runAssignment headF "BC"
        (many (symbol Red *> children (symbol Green *> source)
           <|> symbol Blue *> source))
        (makeState [ node Red 0 1 [ node Green 0 1 [] ]
                   , node Blue 1 2 [] ])
      `shouldBe`
        Some (["B", "C"] :| [])

    it "matches multiple nested children" $
      fst <$> runAssignment headF "12"
        (symbol Red *> children (many (symbol Green *> children (symbol Blue *> source))))
        (makeState [ node Red 0 2 [ node Green 0 1 [ node Blue 0 1 [] ]
                                  , node Green 1 2 [ node Blue 1 2 [] ] ] ])
      `shouldBe`
        Some (["1", "2"] :| [])

  describe "runAssignment" $ do
    it "drops anonymous nodes before matching symbols" $
      fst <$> runAssignment headF "magenta red" red (makeState [node Magenta 0 7 [], node Red 8 11 []])
      `shouldBe`
        Some (Out "red" :| [])

    it "does not drop anonymous nodes after matching" $
      stateNodes . snd <$> runAssignment headF "red magenta" red (makeState [node Red 0 3 [], node Magenta 4 11 []])
      `shouldBe`
        Some ([node Magenta 4 11 []] :| [])

    it "does not drop anonymous nodes when requested" $
      fst <$> runAssignment headF "magenta red" ((,) <$> magenta <*> red) (makeState [node Magenta 0 7 [], node Red 8 11 []])
      `shouldBe`
        Some ((Out "magenta", Out "red") :| [])

    it "produces errors with callstacks pointing at the failing assignment" $
      first (fmap fst . getCallStack . errorCallStack) (runAssignment headF "blue" red (makeState [node Blue 0 4 []]))
      `shouldBe`
      None [ "symbol", "red" ]

node :: symbol -> Int -> Int -> [AST symbol] -> AST symbol
node symbol start end children = Node symbol (Range start end) (Info.Span (Info.Pos 1 (succ start)) (Info.Pos 1 (succ end))) :< children

data Grammar = Palette | Red | Green | Blue | Magenta
  deriving (Enum, Eq, Show)

instance Symbol Grammar where
  symbolType Magenta = Anonymous
  symbolType _ = Regular

data Out = Out B.ByteString | OutError B.ByteString
  deriving (Eq, Show)

red :: HasCallStack => Assignment (AST Grammar) Grammar Out
red = Out <$ symbol Red <*> source

green :: HasCallStack => Assignment (AST Grammar) Grammar Out
green = Out <$ symbol Green <*> source

blue :: HasCallStack => Assignment (AST Grammar) Grammar Out
blue = Out <$ symbol Blue <*> source

magenta :: HasCallStack => Assignment (AST Grammar) Grammar Out
magenta = Out <$ symbol Magenta <*> source

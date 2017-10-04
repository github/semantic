{-# LANGUAGE TypeOperators, DataKinds #-}
module Semantic.Util where

import Data.Blob
import Language.Haskell.HsColour (hscolour, Output(TTY))
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import Text.Show.Pretty (ppShow)
import Files
import Data.Record
import Data.Functor.Classes
import Algorithm
import Data.Align.Generic
import Interpreter
import Parser
import Data.Functor.Both
import Data.Term
import Data.Diff
import Semantic
import Semantic.Task
import Renderer.TOC
import Data.Union
import Data.Syntax.Declaration as Declaration
import Data.Range
import Data.Span
import Data.Syntax

-- Produces colorized pretty-printed output for the terminal / GHCi.
pp :: Show a => a -> IO ()
pp = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow

file :: FilePath -> IO Blob
file path = Files.readFile path (languageForFilePath path)

diffWithParser :: (HasField fields Data.Span.Span,
                   HasField fields Range,
                   Error :< fs,
                   Context :< fs,
                   Declaration.Method :< fs,
                   Declaration.Function :< fs,
                   Declaration.Class :< fs,
                   Empty :< fs,
                   Apply Eq1 fs, Apply Show1 fs,
                   Apply Traversable fs, Apply Functor fs,
                   Apply Foldable fs, Apply Diffable fs,
                   Apply GAlign fs)
                  =>
                  Parser (Term (Data.Union.Union fs) (Record fields))
                  -> Both Blob
                  -> Task (Diff (Union fs) (Record (Maybe Declaration ': fields)) (Record (Maybe Declaration ': fields)))
diffWithParser parser = run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))
  where
    run parse sourceBlobs = distributeFor sourceBlobs parse >>= runBothWith (diffTermPair sourceBlobs diffTerms)

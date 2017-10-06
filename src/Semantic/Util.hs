-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE DataKinds, MonoLocalBinds, TypeOperators #-}
module Semantic.Util where

import Control.Monad.IO.Class
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
import Data.Range
import Data.Span

-- Produces colorized pretty-printed output for the terminal / GHCi.
pp :: Show a => a -> IO ()
pp = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow

file :: MonadIO m => FilePath -> m Blob
file path = Files.readFile path (languageForFilePath path)

diffWithParser :: (HasField fields Data.Span.Span,
                   HasField fields Range,
                   Eq1 syntax, Show1 syntax,
                   Traversable syntax, Functor syntax,
                   Foldable syntax, Diffable syntax,
                   GAlign syntax, HasDeclaration syntax syntax)
                  =>
                  Parser (Term syntax (Record fields))
                  -> Both Blob
                  -> Task (Diff syntax (Record (Maybe Declaration ': fields)) (Record (Maybe Declaration ': fields)))
diffWithParser parser = run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))
  where
    run parse sourceBlobs = distributeFor sourceBlobs parse >>= runBothWith (diffTermPair sourceBlobs diffTerms)

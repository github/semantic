-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE DataKinds, MonoLocalBinds, TypeOperators, TypeApplications #-}
module Semantic.Util where

import Analysis.Declaration
import Analysis.Abstract.Evaluating
import Control.Monad.IO.Class
import Data.Align.Generic
import Data.Maybe
import Data.Blob
import Data.Diff
import Data.Functor.Classes
import Data.Bifunctor.Join
import Data.Range
import Data.Record
import Data.Span
import Data.Term
import Diffing.Algorithm
import Diffing.Interpreter
import Parsing.Parser
import Semantic
import Semantic.IO as IO
import Semantic.Task

import Data.AST
import Data.Union
import Data.Abstract.Value
import Data.Abstract.Address
import qualified Language.Python.Assignment as Python

type PythonValue = Value Precise (Term (Union Python.Syntax) (Record Location))

file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (languageForFilePath path)

parsePythonFiles :: [FilePath] -> FilePath -> IO ([(Blob, Python.Term)], (Blob, Python.Term))
parsePythonFiles paths entryPath = do
  blobs <- traverse file paths
  terms <- runTask $ traverse (parse pythonParser) blobs
  entryBlob <- file entryPath
  entryTerm <- runTask (parse pythonParser entryBlob)
  pure ((zip blobs terms), (entryBlob, entryTerm))

evaluatePythonFiles paths = do
  blobs@(b:bs) <- traverse file paths
  (t:ts) <- runTask $ traverse (parse pythonParser) blobs
  pure $ evaluates @PythonValue (zip bs ts) (b, t)

diffWithParser :: (HasField fields Data.Span.Span,
                   HasField fields Range,
                   Eq1 syntax, Show1 syntax,
                   Traversable syntax, Functor syntax,
                   Foldable syntax, Diffable syntax,
                   GAlign syntax, HasDeclaration syntax)
                  =>
                  Parser (Term syntax (Record fields))
                  -> BlobPair
                  -> Task (Diff syntax (Record (Maybe Declaration ': fields)) (Record (Maybe Declaration ': fields)))
diffWithParser parser = run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))
  where
    run parse blobs = bidistributeFor (runJoin blobs) parse parse >>= diffTermPair diffTerms

diffBlobWithParser :: (HasField fields Data.Span.Span,
                   HasField fields Range,
                   Eq1 syntax, Show1 syntax,
                   Traversable syntax, Functor syntax,
                   Foldable syntax, Diffable syntax,
                   GAlign syntax, HasDeclaration syntax)
                  => Parser (Term syntax (Record fields))
                  -> Blob
                  -> Task (Term syntax (Record (Maybe Declaration : fields)))
diffBlobWithParser parser = run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))
  where
    run parse = parse

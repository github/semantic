-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE DataKinds, MonoLocalBinds, ScopedTypeVariables, TypeFamilies, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import Analysis.Abstract.Caching
import Analysis.Abstract.Dead
import Analysis.Abstract.Evaluating
import Analysis.Abstract.Tracing
import Analysis.Declaration
import Control.Abstract.Analysis
import Control.Monad.IO.Class
import Data.Abstract.Evaluatable
import Data.Abstract.Address
import Data.Abstract.Type
import Data.Abstract.Value
import Data.Blob
import Data.Diff
import Data.Range
import Data.Record
import Data.Span
import Data.Term
import Diffing.Algorithm
import Diffing.Interpreter
import Parsing.Parser
import Prologue
import Semantic
import Semantic.IO as IO
import Semantic.Task

import qualified Language.Go.Assignment as Go
import qualified Language.Python.Assignment as Python
import qualified Language.TypeScript.Assignment as TypeScript

-- Ruby
evaluateRubyFile = evaluateFile rubyParser
evaluateRubyFiles = evaluateFiles rubyParser

-- Go
evaluateGoFile = evaluateFile goParser
evaluateGoFiles = evaluateFiles goParser
typecheckGoFile path = runAnalysis @(Caching Evaluating Go.Term Type) . evaluateModule . snd <$> parseFile goParser path

-- Python
evaluatePythonFile path = evaluate . snd <$> parseFile pythonParser path
evaluatePythonFiles = evaluateFiles pythonParser
typecheckPythonFile path = runAnalysis @(Caching Evaluating Python.Term Type) . evaluateModule . snd <$> parseFile pythonParser path
tracePythonFile path = runAnalysis @(Tracing [] Evaluating Python.Term (Value Precise)) . evaluateModule . snd <$> parseFile pythonParser path
evaluateDeadTracePythonFile path = runAnalysis @(DeadCode (Tracing [] Evaluating) Python.Term (Value Precise)) . evaluateModule . snd <$> parseFile pythonParser path

-- TypeScript
typecheckTypeScriptFile path = runAnalysis @(Caching Evaluating TypeScript.Term Type) . evaluateModule . snd <$> parseFile typescriptParser path
evaluateTypeScriptFile = evaluateFile typescriptParser
evaluateTypeScriptFiles = evaluateFiles typescriptParser

-- Evalute a single file.
evaluateFile :: forall term effects
             .  ( Evaluatable (Base term)
                , FreeVariables term
                , effects ~ RequiredEffects term (Value Precise) (Evaluating term (Value Precise) effects)
                , MonadAddressable Precise (Value Precise) (Evaluating term (Value Precise) effects)
                , MonadValue (Value Precise) (Evaluating term (Value Precise) effects)
                , Recursive term
                )
             => Parser term
             -> FilePath
             -> IO (Final effects (Value Precise))
evaluateFile parser path = evaluate . snd <$> parseFile parser path

-- Evaluate a list of files (head of file list is considered the entry point).
evaluateFiles :: forall term effects
              .  ( Evaluatable (Base term)
                 , FreeVariables term
                 , effects ~ RequiredEffects term (Value Precise) (Evaluating term (Value Precise) effects)
                 , MonadAddressable Precise (Value Precise) (Evaluating term (Value Precise) effects)
                 , MonadValue (Value Precise) (Evaluating term (Value Precise) effects)
                 , Recursive term
                 )
              => Parser term
              -> [FilePath]
              -> IO (Final effects (Value Precise))
evaluateFiles parser paths = do
  entry:xs <- traverse (parseFile parser) paths
  pure $ evaluates @(Value Precise) xs entry

-- Read and parse a file.
parseFile :: Parser term -> FilePath -> IO (Blob, term)
parseFile parser path = runTask $ do
  blob <- file path
  (,) blob <$> parse parser blob

-- Read a file from the filesystem into a Blob.
file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (languageForFilePath path)

-- Diff helpers
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

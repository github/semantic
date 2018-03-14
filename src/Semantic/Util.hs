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
import Data.AST
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
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript

type PreciseValue a = Value Precise (Term (Union a) (Record Location))

type GoValue         = PreciseValue Go.Syntax
type RubyValue       = PreciseValue Ruby.Syntax
type PythonValue     = PreciseValue Python.Syntax
type TypeScriptValue = PreciseValue TypeScript.Syntax

-- Ruby
evaluateRubyFile = evaluateFile @RubyValue rubyParser
evaluateRubyFiles = evaluateFiles @RubyValue rubyParser

-- Go
evaluateGoFile = evaluateFile @GoValue goParser
evaluateGoFiles = evaluateFiles @GoValue goParser
typecheckGoFile path = runAnalysis @(Caching Evaluating Go.Term Type) . evaluateModule . snd <$> parseFile goParser path

-- Python
evaluatePythonFile path = evaluate @PythonValue . snd <$> parseFile pythonParser path
evaluatePythonFiles = evaluateFiles @PythonValue pythonParser
typecheckPythonFile path = runAnalysis @(Caching Evaluating Python.Term Type) . evaluateModule . snd <$> parseFile pythonParser path
tracePythonFile path = runAnalysis @(Tracing [] Evaluating Python.Term PythonValue) . evaluateModule . snd <$> parseFile pythonParser path
evaluateDeadTracePythonFile path = runAnalysis @(DeadCode (Tracing [] Evaluating) Python.Term PythonValue) . evaluateModule . snd <$> parseFile pythonParser path

-- TypeScript
evaluateTypeScriptFile = evaluateFile @TypeScriptValue typescriptParser
evaluateTypeScriptFiles = evaluateFiles @TypeScriptValue typescriptParser

-- Evalute a single file.
evaluateFile :: forall value term effects
             .  ( Evaluatable (Base term)
                , FreeVariables term
                , effects ~ RequiredEffects term value (Evaluating term value effects)
                , MonadAddressable (LocationFor value) value (Evaluating term value effects)
                , MonadValue term value (Evaluating term value effects)
                , Recursive term
                )
             => Parser term
             -> FilePath
             -> IO (Final effects value)
evaluateFile parser path = runAnalysis @(Evaluating term value) . evaluateModule . snd <$> parseFile parser path

-- Evaluate a list of files (head of file list is considered the entry point).
evaluateFiles :: forall value term effects
              .  ( Evaluatable (Base term)
                 , FreeVariables term
                 , effects ~ RequiredEffects term value (Evaluating term value effects)
                 , MonadAddressable (LocationFor value) value (Evaluating term value effects)
                 , MonadValue term value (Evaluating term value effects)
                 , Recursive term
                 )
              => Parser term
              -> [FilePath]
              -> IO (Final effects value)
evaluateFiles parser paths = do
  entry:xs <- traverse (parseFile parser) paths
  pure $ evaluates @value xs entry

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

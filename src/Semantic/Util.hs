-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE DataKinds, MonoLocalBinds, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import Analysis.Abstract.Caching
import Analysis.Abstract.Dead
import Analysis.Abstract.Evaluating
import Analysis.Abstract.Tracing
import Analysis.Declaration
import Control.Abstract.Analysis
import Control.Monad.IO.Class
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

type Language a = Value Precise (Term (Union a) (Record Location))

type GoValue         = Language Go.Syntax
type RubyValue       = Language Ruby.Syntax
type PythonValue     = Language Python.Syntax
type TypeScriptValue = Language TypeScript.Syntax

file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (languageForFilePath path)

-- Ruby
evaluateRubyFile path = fst . evaluate @RubyValue . snd <$> parseFile rubyParser path

evaluateRubyFiles paths = do
  first:rest <- traverse (parseFile rubyParser) paths
  pure $ evaluates @RubyValue rest first

-- Go
typecheckGoFile path = runAnalysis @(Caching Evaluating Go.Term (Type [])) . evaluateModule . snd <$>
  parseFile goParser path

evaluateGoFile path = runAnalysis @(Evaluating Go.Term GoValue) . evaluateModule . snd <$>
  parseFile goParser path

-- Python
typecheckPythonFile path = runAnalysis @(Caching Evaluating Python.Term (Type [])) . evaluateModule . snd <$> parseFile pythonParser path

tracePythonFile path = runAnalysis @(Tracing [] Evaluating Python.Term PythonValue) . evaluateModule . snd <$> parseFile pythonParser path

evaluateDeadTracePythonFile path = runAnalysis @(DeadCode (Tracing [] Evaluating) Python.Term PythonValue) . evaluateModule . snd <$> parseFile pythonParser path

evaluatePythonFile path = evaluate @PythonValue . snd <$> parseFile pythonParser path

evaluatePythonFiles paths = do
  first:rest <- traverse (parseFile pythonParser) paths
  pure $ evaluates @PythonValue rest first

-- TypeScript
typecheckTypeScriptFile path = runAnalysis @(Caching Evaluating TypeScript.Term (Type [])) . evaluateModule . snd <$> parseFile typescriptParser path
evaluateTypeScriptFile path = fst . evaluate @TypeScriptValue . snd <$> parseFile typescriptParser path

evaluateTypeScriptFiles paths = do
  first:rest <- traverse (parseFile typescriptParser) paths
  pure $ evaluates @TypeScriptValue rest first


parseFile :: Parser term -> FilePath -> IO (Blob, term)
parseFile parser path = runTask $ do
  blob <- file path
  (,) blob <$> parse parser blob


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

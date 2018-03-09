-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE DataKinds, MonoLocalBinds, TypeApplications, TypeOperators #-}
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

import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript

type RubyValue = Value Precise (Term (Union Ruby.Syntax) (Record Location))
type PythonValue = Value Precise (Term (Union Python.Syntax) (Record Location))
type TypeScriptValue = Value Precise (Term (Union TypeScript.Syntax) (Record Location))

file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (languageForFilePath path)

-- Ruby
evaluateRubyFile path = Prelude.fst . evaluate @RubyValue . snd <$> parseFile rubyParser path

evaluateRubyFiles paths = do
  first:rest <- traverse (parseFile rubyParser) paths
  pure $ evaluates @RubyValue rest first

-- Python
typecheckPythonFile path = runAnalysis @(CachingAnalysis Evaluating Python.Term Type) . evaluateModule . snd <$> parseFile pythonParser path

tracePythonFile path = runAnalysis @(Tracing [] Evaluating Python.Term PythonValue) . evaluateModule . snd <$> parseFile pythonParser path

evaluateDeadTracePythonFile path = runAnalysis @(DeadCode (Tracing [] Evaluating) Python.Term PythonValue) . evaluateModule . snd <$> parseFile pythonParser path

evaluatePythonFile path = evaluate @PythonValue . snd <$> parseFile pythonParser path

evaluatePythonFiles paths = do
  first:rest <- traverse (parseFile pythonParser) paths
  pure $ evaluates @PythonValue rest first

-- TypeScript
evaluateTypeScriptFile path = fst . evaluate @TypeScriptValue . snd <$> parseFile typescriptParser path

evaluateTypeScriptFiles paths = do
  first:rest <- traverse (parseFile typescriptParser) paths
  pure $ evaluates @TypeScriptValue rest first


parseFile parser path = runTask (file path >>= fmap . (,) <*> parse parser)


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

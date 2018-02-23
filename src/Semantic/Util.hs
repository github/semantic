-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE DataKinds, MonoLocalBinds, TypeOperators, TypeApplications #-}
module Semantic.Util where


import Analysis.Abstract.Evaluating
import Analysis.Declaration
import Control.Monad.IO.Class
import Data.Abstract.Address
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Align.Generic
import Data.AST
import Data.Bifunctor.Join
import Data.Blob
import Data.Diff
import Data.Functor.Classes
import Data.Maybe
import Data.Range
import Data.Record
import Data.Span
import Data.Term
import Data.Union
import Diffing.Algorithm
import Diffing.Interpreter
import Parsing.Parser
import Semantic
import Semantic.IO as IO
import Semantic.Task

import qualified Language.Ruby.Assignment as Ruby
import qualified Language.Python.Assignment as Python

type RubyValue = Value Precise (Term (Union Ruby.Syntax2) (Record Location))
type PythonValue = Value Precise (Term (Union Python.Syntax2) (Record Location))

file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (languageForFilePath path)

-- Ruby
evaluateRubyFile path = Prelude.fst . evaluate @RubyValue <$>
  (file path >>= runTask . parse rubyParser2)

evaluateRubyFiles paths = do
  blobs@(b:bs) <- traverse file paths
  (t:ts) <- runTask $ traverse (parse rubyParser2) blobs
  pure $ evaluates @RubyValue (zip bs ts) (b, t)


-- Python
evaluatePythonFile path = evaluate @PythonValue <$>
  (file path >>= runTask . parse pythonParser2)

evaluatePythonFiles paths = do
  blobs@(b:bs) <- traverse file paths
  (t:ts) <- runTask $ traverse (parse pythonParser2) blobs
  pure $ evaluates @PythonValue (zip bs ts) (b, t)


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

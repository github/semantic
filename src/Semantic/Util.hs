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
import qualified GHC.TypeLits as TypeLevel
import Language.Preluded
import Parsing.Parser
import Prologue
import Semantic
import Semantic.IO as IO
import Semantic.Task

import qualified Language.Go.Assignment as Go
import qualified Language.Python.Assignment as Python
import qualified Language.TypeScript.Assignment as TypeScript

-- Ruby
evaluateRubyFile = evaluateWithPrelude rubyParser
evaluateRubyFiles = evaluateFilesWithPrelude rubyParser

-- Go
evaluateGoFile = evaluateFile goParser
evaluateGoFiles = evaluateFiles goParser
typecheckGoFile path = runAnalysis @(Caching Evaluating Go.Term Type) . evaluateModule . snd <$> parseFile goParser path

-- Python
evaluatePythonFile = evaluateWithPrelude pythonParser
evaluatePythonFiles = evaluateFiles pythonParser
typecheckPythonFile path = runAnalysis @(Caching Evaluating Python.Term Type) . evaluateModule . snd <$> parseFile pythonParser path
tracePythonFile path = runAnalysis @(Tracing [] Evaluating Python.Term Value) . evaluateModule . snd <$> parseFile pythonParser path
evaluateDeadTracePythonFile path = runAnalysis @(DeadCode (Tracing [] Evaluating) Python.Term Value) . evaluateModule . snd <$> parseFile pythonParser path

-- PHP
evaluatePHPFile = evaluateFile phpParser
evaluatePHPFiles = evaluateFiles phpParser

-- TypeScript
typecheckTypeScriptFile path = runAnalysis @(Caching Evaluating TypeScript.Term Type) . evaluateModule . snd <$> parseFile typescriptParser path
evaluateTypeScriptFile = evaluateFile typescriptParser
evaluateTypeScriptFiles = evaluateFiles typescriptParser

-- Evalute a single file.
evaluateFile :: forall term effects
             .  ( Evaluatable (Base term)
                , FreeVariables term
                , effects ~ RequiredEffects term Value (Evaluating term Value effects)
                , MonadAddressable Precise Value (Evaluating term Value effects)
                , MonadValue Value (Evaluating term Value effects)
                , Recursive term
                )
             => Parser term
             -> FilePath
             -> IO (Final effects Value)
evaluateFile parser path = evaluate . snd <$> parseFile parser path

evaluateWithPrelude :: forall term effects
                    .  ( Evaluatable (Base term)
                       , FreeVariables term
                       , effects ~ RequiredEffects term Value (Evaluating term Value effects)
                       , MonadAddressable Precise Value (Evaluating term Value effects)
                       , MonadValue Value (Evaluating term Value effects)
                       , Recursive term
                       , TypeLevel.KnownSymbol (PreludePath term)
                       )
                    => Parser term
                    -> FilePath
                    -> IO (Final effects Value)
evaluateWithPrelude parser path = do
  let preludePath = TypeLevel.symbolVal (Proxy :: Proxy (PreludePath term))
  prelude <- parseFile parser preludePath
  blob <- parseFile parser path
  pure $ evaluateWith (snd prelude) (snd blob)

-- Evaluate a list of files (head of file list is considered the entry point).
evaluateFiles :: forall term effects
              .  ( Evaluatable (Base term)
                 , FreeVariables term
                 , effects ~ RequiredEffects term Value (Evaluating term Value effects)
                 , MonadAddressable Precise Value (Evaluating term Value effects)
                 , MonadValue Value (Evaluating term Value effects)
                 , Recursive term
                 )
              => Parser term
              -> [FilePath]
              -> IO (Final effects Value)
evaluateFiles parser paths = do
  entry:xs <- traverse (parseFile parser) paths
  pure $ evaluates @Value xs entry

evaluateFilesWithPrelude :: forall term effects
                         .  ( Evaluatable (Base term)
                            , FreeVariables term
                            , effects ~ RequiredEffects term Value (Evaluating term Value effects)
                            , MonadAddressable Precise Value (Evaluating term Value effects)
                            , MonadValue Value (Evaluating term Value effects)
                            , Recursive term
                            , TypeLevel.KnownSymbol (PreludePath term)
                            )
                         => Parser term
                         -> [FilePath]
                         -> IO (Final effects Value)
evaluateFilesWithPrelude parser paths = do
  let preludePath = TypeLevel.symbolVal (Proxy :: Proxy (PreludePath term))
  prelude <- parseFile parser preludePath
  entry:xs <- traverse (parseFile parser) paths
  pure $ evaluatesWith @Value (snd prelude) xs entry

-- Read and parse a file.
parseFile :: Parser term -> FilePath -> IO (Blob, term)
parseFile parser path = runTask $ do
  blob <- file path
  (,) blob <$> parse parser blob

-- Read a file from the filesystem into a Blob.
file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (languageForFilePath path)

-- Diff helpers
diffWithParser ::
               ( HasField fields Data.Span.Span
               , HasField fields Range
               , Eq1 syntax
               , Show1 syntax
               , Traversable syntax
               , Diffable syntax
               , GAlign syntax
               , HasDeclaration syntax
               )
               => Parser (Term syntax (Record fields))
               -> BlobPair
               -> Task (Diff syntax (Record (Maybe Declaration ': fields)) (Record (Maybe Declaration ': fields)))
diffWithParser parser = run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))
  where
    run parse blobs = bidistributeFor (runJoin blobs) parse parse >>= diffTermPair diffTerms

diffBlobWithParser ::
                   ( HasField fields Data.Span.Span
                   , HasField fields Range
                   , Traversable syntax
                   , HasDeclaration syntax
                   )
                  => Parser (Term syntax (Record fields))
                  -> Blob
                  -> Task (Term syntax (Record (Maybe Declaration : fields)))
diffBlobWithParser parser = run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))
  where
    run parse = parse

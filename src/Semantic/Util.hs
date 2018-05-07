{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Evaluating as X
import           Control.Abstract.Evaluator
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Value
import           Data.Abstract.Type
import           Data.Blob
import           Data.File
import qualified Data.Language as Language
import qualified GHC.TypeLits as TypeLevel
import           Language.Preluded
import           Parsing.Parser
import           Prologue
import           Semantic.Graph
import           Semantic.IO as IO
import           Semantic.Task

import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby

justEvaluating
  = run
  . evaluating
  . runLoadError
  . runValueError
  . runUnspecialized
  . runResolutionError
  . runEvalError
  . runAddressError

evaluatingWithHoles
  = run
  . evaluating
  . resumingLoadError
  . resumingUnspecialized
  . resumingValueError
  . resumingEvalError
  . resumingResolutionError
  . resumingAddressError @(Value Precise)

-- The order is significant here: caching has to run before typeChecking, or else we’ll nondeterministically produce TypeErrors as part of the result set. While this is probably actually correct, it will require us to have an Ord instance for TypeError, which we don’t have yet.
checking
  = run
  . evaluating
  . providingLiveSet
  . runLoadError
  . runUnspecialized
  . runResolutionError
  . runEvalError
  . runAddressError
  . runTypeError
  . caching @[]

evalGoProject path = justEvaluating <$> evaluateProject goParser Language.Go Nothing path
evalRubyProject path = justEvaluating <$> evaluateProject rubyParser Language.Ruby rubyPrelude path
evalPHPProject path = justEvaluating <$> evaluateProject phpParser Language.PHP Nothing path
evalPythonProject path = justEvaluating <$> evaluateProject pythonParser Language.Python pythonPrelude path
evalTypeScriptProjectQuietly path = evaluatingWithHoles <$> evaluateProject typescriptParser Language.TypeScript Nothing path
evalTypeScriptProject path = justEvaluating <$> evaluateProject typescriptParser Language.TypeScript Nothing path

typecheckGoFile path = checking <$> evaluateProjectWithCaching goParser Language.Go Nothing path

rubyPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Ruby.Term))) (Just Language.Ruby)
pythonPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Python.Term))) (Just Language.Python)

-- Evaluate a project, starting at a single entrypoint.
evaluateProject parser lang prelude path = evaluatePackageWith id id <$> runTask (readProject Nothing path lang [] >>= parsePackage parser prelude)
evaluateProjectWithCaching parser lang prelude path = evaluatePackageWith convergingModules cachingTerms <$> runTask (readProject Nothing path lang [] >>= parsePackage parser prelude)


parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)

blob :: FilePath -> IO Blob
blob = runTask . readBlob . file

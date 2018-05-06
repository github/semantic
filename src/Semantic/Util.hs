{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import           Analysis.Abstract.BadAddresses
import           Analysis.Abstract.BadModuleResolutions
import           Analysis.Abstract.BadSyntax
import           Analysis.Abstract.BadValues
import           Analysis.Abstract.BadVariables
import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Erroring
import           Analysis.Abstract.Evaluating as X
import           Analysis.Abstract.TypeChecking
import           Control.Abstract.Evaluator
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Type
import           Data.Abstract.Value
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
  = evaluating @(Value Precise)
  . failingOnLoadErrors
  . erroring @(ValueError Precise (Value Precise))
  . erroring @(Unspecialized (Value Precise))
  . erroring @ResolutionError
  . erroring @(EvalError (Value Precise))
  . erroring @(AddressError Precise (Value Precise))

evaluatingWithHoles
  = evaluating @(Value Precise)
  . failingOnLoadErrors
  . resumingBadSyntax @(Value Precise)
  . resumingBadValues @(Value Precise)
  . resumingBadVariables @(Value Precise)
  . resumingBadModuleResolutions
  . resumingBadAddresses @(Value Precise)

-- The order is significant here: Caching has to come on the outside, or its Interpreter instance
-- will expect the TypeError exception type to have an Ord instance, which is wrong.
checking
  = evaluating @(Type Monovariant)
  . providingLiveSet
  . failingOnLoadErrors
  . erroring @(Unspecialized (Type Monovariant))
  . erroring @ResolutionError
  . erroring @(EvalError (Type Monovariant))
  . erroring @(AddressError Monovariant (Type Monovariant))
  . typeChecking
  . caching @[]

failingOnLoadErrors :: Evaluator location term value (Resumable (LoadError term) ': effects) a -> Evaluator location term value effects (Either (SomeExc (LoadError term)) a)
failingOnLoadErrors = erroring

evalGoProject path = justEvaluating <$> evaluateProject goParser Language.Go Nothing path
evalRubyProject path = justEvaluating <$> evaluateProject rubyParser Language.Ruby rubyPrelude path
evalPHPProject path = justEvaluating <$> evaluateProject phpParser Language.PHP Nothing path
evalPythonProject path = justEvaluating <$> evaluateProject pythonParser Language.Python pythonPrelude path
evalTypeScriptProjectQuietly path = evaluatingWithHoles <$> evaluateProject typescriptParser Language.TypeScript Nothing path
evalTypeScriptProject path = justEvaluating <$> evaluateProject typescriptParser Language.TypeScript Nothing path

typecheckGoFile path = checking <$> evaluateProject goParser Language.Go Nothing path

rubyPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Ruby.Term))) (Just Language.Ruby)
pythonPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Python.Term))) (Just Language.Python)

-- Evaluate a project, starting at a single entrypoint.
evaluateProject parser lang prelude path = evaluatePackageWith id id <$> runTask (readProject Nothing path lang [] >>= parsePackage parser prelude)


parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)

blob :: FilePath -> IO Blob
blob = runTask . readBlob . file

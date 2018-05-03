{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import           Analysis.Abstract.BadAddresses
import           Analysis.Abstract.BadModuleResolutions
import           Analysis.Abstract.BadSyntax
import           Analysis.Abstract.BadValues
import           Analysis.Abstract.BadLoads
import           Analysis.Abstract.BadVariables
import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Erroring
import           Analysis.Abstract.Evaluating as X
import           Analysis.Abstract.TypeChecking
import           Analysis.Abstract.PythonPackage
import           Control.Abstract.Analysis
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Located
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

import qualified Language.Go.Assignment as Go
import qualified Language.PHP.Assignment as PHP
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript

type JustEvaluating term
  = Erroring (AddressError (Located Precise term) (Value (Located Precise term)))
  ( Erroring (EvalError (Value (Located Precise term)))
  ( Erroring (ResolutionError (Value (Located Precise term)))
  ( Erroring (Unspecialized (Value (Located Precise term)))
  ( Erroring (ValueError (Located Precise term) (Value (Located Precise term)))
  ( Erroring (LoadError term)
  ( Evaluating (Located Precise term) term (Value (Located Precise term))))))))

type EvaluatingWithHoles term
  = BadAddresses
  ( BadModuleResolutions
  ( BadVariables
  ( BadValues
  ( BadSyntax
  ( BadLoads
  ( Evaluating (Located Precise term) term (Value (Located Precise term))))))))

-- The order is significant here: Caching has to come on the outside, or its Interpreter instance
-- will expect the TypeError exception type to have an Ord instance, which is wrong.
type Checking term
  = Caching
  ( TypeChecking
  ( Erroring (AddressError Monovariant Type)
  ( Erroring (EvalError Type)
  ( Erroring (ResolutionError Type)
  ( Erroring (Unspecialized Type)
  ( Erroring (LoadError term)
  ( Retaining
  ( Evaluating Monovariant term Type))))))))

evalGoProject path = interpret @(JustEvaluating Go.Term) <$> evaluateProject goParser Language.Go Nothing path
evalRubyProject path = interpret @(JustEvaluating Ruby.Term) <$> evaluateProject rubyParser Language.Ruby rubyPrelude path
evalPHPProject path = interpret @(JustEvaluating PHP.Term) <$> evaluateProject phpParser Language.PHP Nothing path
evalPythonProject path = interpret @(PythonPackaging (EvaluatingWithHoles Python.Term)) <$> evaluatePythonProject pythonParser Language.Python pythonPrelude path
evalTypeScriptProjectQuietly path = interpret @(EvaluatingWithHoles TypeScript.Term) <$> evaluateProject typescriptParser Language.TypeScript Nothing path
evalTypeScriptProject path = interpret @(JustEvaluating TypeScript.Term) <$> evaluateProject typescriptParser Language.TypeScript Nothing path

typecheckGoFile path = interpret @(Checking Go.Term) <$> evaluateProject goParser Language.Go Nothing path

rubyPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Ruby.Term))) (Just Language.Ruby)
pythonPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Python.Term))) (Just Language.Python)

-- Evaluate a project, starting at a single entrypoint.
evaluateProject parser lang prelude path = evaluatePackage <$> runTask (readProject Nothing path lang [] >>= parsePackage parser prelude)
evaluatePythonProject parser lang prelude path = evaluatePackage <$> runTask (readProject Nothing path lang [] >>= parsePythonPackage parser prelude)

evalRubyFile path = interpret @(JustEvaluating Ruby.Term) <$> evaluateFile rubyParser path
evaluateFile parser path = evaluateModule <$> runTask (parseModule parser Nothing (file path))


parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)

blob :: FilePath -> IO Blob
blob = runTask . readBlob . file

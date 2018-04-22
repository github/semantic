-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE DataKinds, MonoLocalBinds, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Semantic.Util where

import Analysis.Abstract.BadVariables
import Analysis.Abstract.BadModuleResolutions
import Analysis.Abstract.BadValues
import Analysis.Abstract.Caching
import Analysis.Abstract.Quiet
import Analysis.Abstract.Dead
import Analysis.Abstract.Evaluating as X
import Analysis.Abstract.ImportGraph
import Analysis.Abstract.Tracing
import Analysis.Declaration
import Control.Abstract.Analysis
import Control.Monad.IO.Class
import Data.Abstract.Evaluatable hiding (head)
import Data.Abstract.Address
import Data.Abstract.Located
import Data.Abstract.Module
import Data.Abstract.Package as Package
import Data.Abstract.Type
import Data.Abstract.Value
import Data.Blob
import Data.File
import Data.Diff
import Data.Range
import Data.Record
import qualified Data.Language as Language
import Data.Span
import Data.Term
import Diffing.Algorithm
import Diffing.Interpreter
import System.FilePath.Glob
import qualified GHC.TypeLits as TypeLevel
import Language.Preluded
import Parsing.Parser
import Prologue
import Semantic.Diff (diffTermPair)
import Semantic.IO as IO
import Semantic.Task
import Semantic.Graph
import qualified Semantic.Task as Task
import System.FilePath.Posix

import qualified Language.Go.Assignment as Go
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.PHP.Assignment as PHP
import qualified Language.TypeScript.Assignment as TypeScript


-- type TestEvaluating term = Evaluating Precise term (Value Precise)
type JustEvaluating term = Evaluating (Located Precise term) term (Value (Located Precise term))
type EvaluatingWithHoles term = BadModuleResolutions (BadVariables (BadValues (Quietly (Evaluating (Located Precise term) term (Value (Located Precise term))))))
type ImportGraphingWithHoles term = ImportGraphing (EvaluatingWithHoles term)

evalGoProject path = runAnalysis @(JustEvaluating Go.Term) <$> evaluateProject goParser Nothing path
evalRubyProject path = runAnalysis @(JustEvaluating Ruby.Term) <$> evaluateProject rubyParser rubyPrelude path
evalPHPProject path = runAnalysis @(JustEvaluating PHP.Term) <$> evaluateProject phpParser Nothing path
evalPythonProject path = runAnalysis @(JustEvaluating Python.Term) <$> evaluateProject pythonParser pythonPrelude path
evalTypeScriptProject path = runAnalysis @(EvaluatingWithHoles TypeScript.Term) <$> evaluateProject typescriptParser Nothing path
evaluateProject parser prelude path = evaluatePackage <$> runTask (readProject Nothing (file path :| []) >>= parsePackage parser prelude)

rubyPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Ruby.Term))) (Just Language.Ruby)
pythonPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Python.Term))) (Just Language.Python)

-- Read and parse a file.
parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)

-- Read a file from the filesystem into a Blob.
blob :: FilePath -> IO Blob
blob = runTask . readBlob . file

-- Diff helpers
diffWithParser :: ( HasField fields Data.Span.Span
                  , HasField fields Range
                  , Eq1 syntax
                  , Show1 syntax
                  , Traversable syntax
                  , Diffable syntax
                  , GAlign syntax
                  , HasDeclaration syntax
                  , Members '[Distribute WrappedTask, Task] effs
                  )
               => Parser (Term syntax (Record fields))
               -> BlobPair
               -> Eff effs (Diff syntax (Record (Maybe Declaration ': fields)) (Record (Maybe Declaration ': fields)))
diffWithParser parser blobs = distributeFor blobs (\ blob -> WrapTask $ parse parser blob >>= decorate (declarationAlgebra blob)) >>= diffTermPair diffTerms . runJoin

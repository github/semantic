-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE MonoLocalBinds, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import           Analysis.Abstract.BadModuleResolutions
import           Analysis.Abstract.BadValues
import           Analysis.Abstract.BadVariables
import           Analysis.Abstract.Evaluating as X
import           Analysis.Abstract.ImportGraph
import           Analysis.Abstract.Quiet
import           Analysis.Declaration
import           Control.Abstract.Analysis
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Located
import           Data.Abstract.Value
import           Data.Blob
import           Data.Diff
import           Data.File
import qualified Data.Language as Language
import           Data.Range
import           Data.Record
import           Data.Span
import           Data.Term
import           Diffing.Algorithm
import           Diffing.Interpreter
import qualified GHC.TypeLits as TypeLevel
import           Language.Preluded
import           Parsing.Parser
import           Prologue
import           Semantic.Diff (diffTermPair)
import           Semantic.Graph
import           Semantic.IO as IO
import           Semantic.Task

import qualified Language.Go.Assignment as Go
import qualified Language.PHP.Assignment as PHP
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
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

rubyPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Ruby.Term))) (Just Language.Ruby)
pythonPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Python.Term))) (Just Language.Python)

-- Evaluate a project, starting at a single entrypoint.
evaluateProject parser prelude path = evaluatePackage <$> runTask (readProject Nothing (file path :| []) [] >>= parsePackage parser prelude)

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

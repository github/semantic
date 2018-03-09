{-# LANGUAGE TypeApplications #-}
module Analysis.Spec where

import Analysis.Abstract.Caching
import Analysis.Abstract.Evaluating
import Analysis.Declaration
import Control.Monad.IO.Class
import Data.Abstract.Address
import Data.Abstract.Type
import Data.Abstract.Value as Value
import Data.AST
import Data.Blob
import Data.Diff
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

import Data.Map as Map
import Data.Abstract.Store
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.ModuleTable

import SpecHelpers
import Test.Hspec (Spec, describe, it, xit, parallel, pendingWith)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck

import qualified Language.Python.Assignment as Python


spec :: Spec
spec = parallel $ do
  describe "python" $ do
    it "standard imports" $ do
      res <- fst . fst . fst . fst <$> evaluatePythonFiles
        [ "test/fixtures/python/analysis/main.py"
        , "test/fixtures/python/analysis/a.py"
        , "test/fixtures/python/analysis/b/c.py"
        ]

      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["a", "foo"], addr 0)
            , (qualifiedName ["b", "c", "baz"], addr 1)
            ]
      res `shouldBe` Right (inj @(Interface Precise) (Interface (inj (Boolean False)) expectedEnv))

  where
    addr = Address . Precise
    evaluatePythonFiles :: [FilePath]
                         -> IO (
                                 (
                                   (
                                     ( Either
                                         Prelude.String
                                         (Union (ValueConstructors Precise) (Term (Union Python.Syntax) (Record Location)))
                                     , Store Precise PythonValue
                                     )
                                     , Map.Map Data.Abstract.FreeVariables.Name (Data.Abstract.FreeVariables.Name, Maybe (Address Precise PythonValue))
                                   )
                                   , Environment Precise PythonValue
                                 )
                                 , ModuleTable (Environment Precise PythonValue)
                               )
    evaluatePythonFiles paths = do
      blobs@(b:bs) <- traverse file paths
      (t:ts) <- runTask $ traverse (parse pythonParser) blobs
      pure $ evaluates @PythonValue (zip bs ts) (b, t)

type PythonValue = Value Precise (Term (Union Python.Syntax) (Record Location))

file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (IO.languageForFilePath path)

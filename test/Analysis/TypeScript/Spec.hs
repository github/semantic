{-# LANGUAGE TypeApplications #-}
module Analysis.TypeScript.Spec where

import Control.Monad.IO.Class
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

import Data.Semigroup
import Data.Map as Map

import Analysis.Abstract.Caching
import Analysis.Abstract.Evaluating
import Analysis.Declaration
import Data.Abstract.Store
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.ModuleTable
import Data.Abstract.Address
import Data.Abstract.Type
import Data.Abstract.Value as Value

import qualified Language.TypeScript.Assignment as TypeScript

import SpecHelpers
import Test.Hspec (Spec, describe, it, xit, parallel, pendingWith)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck



spec :: Spec
spec = parallel $ do
  describe "evalutes TypeScript" $ do
    it "imports with aliased symbols" $ do
      res <- evaluate "main.ts"

      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["bar"], addr 0)
            ]
      res `shouldBe` Right (inj @(Interface Precise) (Interface (inj (Value.String "\"this is the baz function\"")) expectedEnv))

    it "imports with qualified names" $ do
      res <- evaluate "main1.ts"

      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["b", "baz"], addr 0)
            , (qualifiedName ["b", "foo"], addr 2)
            , (qualifiedName ["z", "baz"], addr 0)
            , (qualifiedName ["z", "foo"], addr 2)
            ]
      res `shouldBe` Right (inj @(Interface Precise) (Interface (inj (Value.String "\"this is the foo function\"")) expectedEnv))

  where
    addr = Address . Precise

    evaluate entry = fst . fst . fst . fst <$> evaluateTypeScriptFiles
      [ "test/fixtures/typescript/analysis/" <> entry
      , "test/fixtures/typescript/analysis/a.ts"
      , "test/fixtures/typescript/analysis/foo.ts"
      ]
    evaluateTypeScriptFiles :: [FilePath]
                         -> IO (
                                 (
                                   (
                                     ( Either
                                         Prelude.String
                                         (Union (ValueConstructors Precise) (Term (Union TypeScript.Syntax) (Record Location)))
                                     , Store Precise TypeScriptValue
                                     )
                                     , Map.Map Data.Abstract.FreeVariables.Name (Data.Abstract.FreeVariables.Name, Maybe (Address Precise TypeScriptValue))
                                   )
                                   , Environment Precise TypeScriptValue
                                 )
                                 , ModuleTable (Environment Precise TypeScriptValue)
                               )
    evaluateTypeScriptFiles paths = do
      blobs@(b:bs) <- traverse file paths
      (t:ts) <- runTask $ traverse (parse typescriptParser) blobs
      pure $ evaluates @TypeScriptValue (zip bs ts) (b, t)

type TypeScriptValue = Value Precise (Term (Union TypeScript.Syntax) (Record Location))

file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (IO.languageForFilePath path)

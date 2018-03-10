{-# LANGUAGE TypeApplications #-}
module Analysis.Python.Spec where

import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Value as Value
import Data.AST
import Data.Map as Map
import Data.Record
import Data.Semigroup
import Data.Term
import Data.Union
import Parsing.Parser

import SpecHelpers
import Test.Hspec (Spec, describe, it, xit, parallel, pendingWith)
import Test.Hspec.Expectations.Pretty
import qualified Language.Python.Assignment as Python


type PythonValue = Value Precise (Term (Union Python.Syntax) (Record Location))

spec :: Spec
spec = parallel $ do
  describe "evalutes Python" $ do
    it "imports" $ do
      res <- evaluate "main.py"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["a", "foo"], addr 0)
            , (qualifiedName ["b", "c", "baz"], addr 1)
            ]
      res `shouldBe` interface expectedEnv

    it "imports with aliases" $ do
      res <- evaluate "main1.py"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["b", "foo"], addr 0)
            , (qualifiedName ["e", "baz"], addr 1)
            ]
      res `shouldBe` interface expectedEnv

    it "imports using 'from' syntax" $ do
      res <- evaluate "main2.py"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["foo"], addr 0)
            , (qualifiedName ["bar"], addr 1)
            ]
      res `shouldBe` interface expectedEnv

  where
    interface e = Right (inj @(Interface Precise) (Interface (inj (Boolean False)) e))
    addr = Address . Precise
    fixtures = "test/fixtures/python/analysis/"
    evaluate entry = fst . fst . fst . fst <$>
      evaluateFiles @PythonValue pythonParser
      [ fixtures <> entry
      , fixtures <> "a.py"
      , fixtures <> "b/c.py"
      ]

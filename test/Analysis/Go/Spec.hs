{-# LANGUAGE TypeApplications #-}
module Analysis.Go.Spec where

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
import qualified Language.Go.Assignment as Go


type GoValue = Value Precise (Term (Union Go.Syntax) (Record Location))

spec :: Spec
spec = parallel $ do
  describe "evalutes Go" $ do
    it "imports and wildcard imports" $ do
      res <- evaluate "main.go"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["foo", "New"], addr 0)
            , (qualifiedName ["Bar"], addr 1)
            , (qualifiedName ["main"], addr 2)
            ]
      assertEnvironment res expectedEnv

    it "imports with aliases (and side effects only)" $ do
      res <- evaluate "main1.go"
      let expectedEnv = Environment $ fromList
            [ (qualifiedName ["f", "New"], addr 0)
            , (qualifiedName ["main"], addr 2) -- side effects of eval'ing `import _ "./bar"` used addr 1.
            ]
      assertEnvironment res expectedEnv

  where
    assertEnvironment result expectedEnv = case result of
      Left e -> expectationFailure ("Evaluating expected to succeed, but failed with: " <> e)
      Right res -> let Just (Interface _ env) = prj @(Interface Precise) res in env `shouldBe` expectedEnv

    addr = Address . Precise
    fixtures = "test/fixtures/go/analysis/"
    evaluate entry = fst . fst . fst . fst <$>
      evaluateFiles @GoValue goParser
        [ fixtures <> entry
        , fixtures <> "foo/foo.go"
        , fixtures <> "bar/bar.go"
        , fixtures <> "bar/rab.go"
        ]

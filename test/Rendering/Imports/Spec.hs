module Rendering.Imports.Spec (spec) where

import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Analysis.ModuleDef (HasModuleDef, moduleDefAlgebra)
import Data.Output
import Parsing.Parser
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Semantic.Util as Util
import Rendering.Imports
import Rendering.Renderer
import Rendering.TOC.Spec hiding (spec)
import Semantic
import Semantic.Task
import SpecHelpers
import Test.Hspec (Spec, describe, it, xit, parallel, pendingWith)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck


spec :: Spec
spec = parallel $ do
  describe "renderToImports" $ do
    it "works for Ruby" $ do
      output <- parseToImports rubyParser "test/fixtures/ruby/import-graph/app.rb"
      expected <- readFileVerbatim "test/fixtures/ruby/import-graph/app.json"
      toVerbatimOutput output `shouldBe` expected

    it "works for Python" $ do
      output <- parseToImports pythonParser "test/fixtures/python/import-graph/main.py"
      expected <- readFileVerbatim "test/fixtures/python/import-graph/main.json"
      toVerbatimOutput output `shouldBe` expected

    it "works for Go" $ do
      output <- parseToImports goParser "test/fixtures/go/import-graph/main.go"
      expected <- readFileVerbatim "test/fixtures/go/import-graph/main.json"
      toVerbatimOutput output `shouldBe` expected

    it "works for TypeScript" $ do
      output <- parseToImports typescriptParser "test/fixtures/typescript/import-graph/app.ts"
      expected <- readFileVerbatim "test/fixtures/typescript/import-graph/app.json"
      toVerbatimOutput output `shouldBe` expected

  where
    toVerbatimOutput = verbatim . toOutput
    parseToImports parser path = do
      blob <- Util.file path
      runTask (parse parser blob >>= decorate (declarationAlgebra blob) >>= decorate (moduleDefAlgebra blob) >>= render (renderToImports blob))

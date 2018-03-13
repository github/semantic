module Rendering.Imports.Spec (spec) where

import Analysis.Declaration (declarationAlgebra)
import Analysis.ModuleDef (moduleDefAlgebra)
import Rendering.Imports
import SpecHelpers


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
      blob <- file path
      runTask (parse parser blob >>= decorate (declarationAlgebra blob) >>= decorate (moduleDefAlgebra blob) >>= render (renderToImports blob))

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import AST.GenerateSyntax
import qualified Bazel.Runfiles as Bazel
import Control.Lens (Traversal', mapped, (%~))
import Control.Monad
import Data.Foldable
import Data.Generics.Product.Typed (typed)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Foreign
import GHC.Generics (Generic)
import Language.Haskell.TH hiding (JavaScript)
import Language.Haskell.TH.Lens
import NeatInterpolation
import qualified Options.Generic as Opt
import Source.Language
import System.FilePath
import System.Directory
import System.Exit
import System.IO
import System.Process
import Text.Printf
import qualified TreeSitter.Go as Go (tree_sitter_go)
import qualified TreeSitter.JSON as JSON (tree_sitter_json)
import qualified TreeSitter.Java as Java (tree_sitter_java)
import qualified TreeSitter.Language
import qualified TreeSitter.PHP as PHP (tree_sitter_php)
import qualified TreeSitter.Python as Python (tree_sitter_python)
import qualified TreeSitter.QL as CodeQL (tree_sitter_ql)
import qualified TreeSitter.Ruby as Ruby (tree_sitter_ruby)
import qualified TreeSitter.TSX as TSX (tree_sitter_tsx)
import qualified TreeSitter.TypeScript as TypeScript (tree_sitter_typescript)

-- As a special case, you can pass
data Config = Config {language :: Text, rootdir :: FilePath}
  deriving stock (Show, Generic)
  deriving anyclass (Opt.ParseRecord)

-- There are a few cases where the output emitted by TH's 'pprint' doesn't
-- create entirely valid Haskell syntax, because sometimes we get
-- a qualified name on the LHS of a typeclass declaration, which Haskell
-- doesn't like at all. I haven't figured out quite why we get this qualified
-- name, but for now the easiest thing to do is some nested updates with lens.
adjust :: Dec -> Dec
adjust = _InstanceD . typed . mapped %~ (values %~ truncate) . (functions %~ truncate)
  where
    -- Need to handle functions with no arguments, which are parsed as ValD entities,
    -- as well as those with arguments, which are FunD.
    values, functions :: Traversal' Dec Name
    values = _ValD . typed . _VarP
    functions = _FunD . typed

    truncate :: Name -> Name
    truncate = mkName . nameBase

pathForLanguage :: Bazel.Runfiles -> Language -> FilePath
pathForLanguage rf =
  let loc = Bazel.rlocation rf
   in \case
        CodeQL -> loc "tree-sitter-ql/vendor/tree-sitter-ql/src/node-types.json"
        Go -> loc "tree-sitter-go/vendor/tree-sitter-go/src/node-types.json"
        PHP -> loc "tree-sitter-php/vendor/tree-sitter-php/src/node-types.json"
        Python -> loc "tree-sitter-python/vendor/tree-sitter-python/src/node-types.json"
        Ruby -> loc "tree-sitter-ruby/vendor/tree-sitter-ruby/src/node-types.json"
        TypeScript -> loc "tree-sitter-typescript/vendor/tree-sitter-typescript/typescript/src/node-types.json"
        TSX -> loc "tree-sitter-tsx/vendor/tree-sitter-typescript/tsx/src/node-types.json"
        JavaScript -> loc "tree-sitter-typescript/vendor/tree-sitter-typescript/typescript/src/node-types.json"
        JSX -> loc "tree-sitter-typescript/vendor/tree-sitter-typescript/src/tsx/node-types.json"
        Java -> loc "tree-sitter-java/vendor/tree-sitter-java/src/node-types.json"
        other -> error ("Couldn't find path for " <> show other)

targetForLanguage :: Language -> FilePath
targetForLanguage x =
  let go lc = printf "semantic-%s/src/Language/%s/AST.hs" (lc :: String) (show x)
   in case x of
        CodeQL -> go "codeql"
        Go -> go "go"
        PHP -> go "php"
        Python -> go "python"
        Ruby -> go "ruby"
        TypeScript -> go "typescript"
        TSX -> go "tsx"
        JavaScript -> go "javascript"
        Java -> go "java"
        other -> error ("Couldn't find path for " <> show other)

parserForLanguage :: Language -> Ptr TreeSitter.Language.Language
parserForLanguage = \case
  Unknown -> error "Unknown language encountered"
  CodeQL -> (CodeQL.tree_sitter_ql)
  Go -> Go.tree_sitter_go
  Haskell -> error "Haskell backend not implemented yet"
  Java -> Java.tree_sitter_java
  JavaScript -> TypeScript.tree_sitter_typescript
  JSON -> JSON.tree_sitter_json
  JSX -> TSX.tree_sitter_tsx
  Markdown -> error "Markdown backend deprecated"
  PHP -> PHP.tree_sitter_php
  Python -> Python.tree_sitter_python
  Ruby -> Ruby.tree_sitter_ruby
  TypeScript -> TypeScript.tree_sitter_typescript
  TSX -> TSX.tree_sitter_tsx

-- nodeTypesPathForLanguage :: Bazel.Runfiles -> Language -> FilePath
-- nodeTypesPathForLanguage rf = \case
--   CodeQL -> r

validLanguages :: [Language]
validLanguages = [CodeQL, Go, Java, PHP, Python, Ruby, TypeScript, TSX]

emit :: FilePath -> Language -> IO ()
emit root lang = do
  rf <- Bazel.create
  let language = languageToText lang
  let languageHack = case lang of
        CodeQL -> "QL"
        _ -> language
  let path = pathForLanguage rf lang
  decls <- T.pack . pprint . fmap adjust <$> astDeclarationsIO (parserForLanguage lang) path

  let programText =
        [trimming|
-- Language definition for $language, generated by ast-generate. Do not edit!
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Language.$language.AST (module Language.$language.AST, getTestCorpusDir) where

import qualified AST.Parse
import qualified AST.Token
import qualified AST.Traversable1.Class
import qualified AST.Unmarshal
import qualified Data.Foldable
import qualified Data.List as Data.OldList
import qualified Data.Maybe as GHC.Maybe
import qualified Data.Text.Internal
import qualified Data.Traversable
import qualified GHC.Base
import qualified GHC.Generics
import qualified GHC.Records
import qualified GHC.Show
import qualified Prelude as GHC.Classes
import qualified TreeSitter.Node

import TreeSitter.$languageHack (getTestCorpusDir)

debugSymbolNames :: [GHC.Base.String]
debugSymbolNames = debugSymbolNames_0

$decls
  |]
  hasOrmolu <- findExecutable "ormolu"
  if isNothing hasOrmolu
    then do
      T.putStrLn programText
      hPutStrLn stderr "generate-ast: No `ormolu` executable found, output will look janky."
    else do
      (path, tf) <- openTempFile "/tmp" "generated.hs"
      T.hPutStrLn tf programText
      hClose tf
      callProcess "ormolu" ["--mode", "inplace", path]
      callProcess "cp" [path, root </> targetForLanguage lang]

main :: IO ()
main = do
  Config language root <- Opt.getRecord "generate-ast"
  if language == "all"
    then traverse_ (emit root) validLanguages
    else do
      let lang = textToLanguage language
      when (lang == Unknown) (die ("Couldn't determine language for " <> T.unpack language))
      emit root lang

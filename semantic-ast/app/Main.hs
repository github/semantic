{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import AST.GenerateSyntax
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Language.Haskell.TH
import NeatInterpolation
import qualified Options.Generic as Opt
import System.Directory
import System.IO
import System.Process
import qualified TreeSitter.JSON as JSON (tree_sitter_json)

data Config = Config
  { language :: Text,
    path :: FilePath
  }
  deriving (Show, Generic)

instance Opt.ParseRecord Config

header :: Text
header =
  [trimming|
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
|]

main :: IO ()
main = do
  Config language path <- Opt.getRecord "generate-ast"
  absolute <- makeAbsolute path
  decls <- runQ (astDeclarationsRelative JSON.tree_sitter_json absolute)

  let modheader =
        [trimming| module Language.$language.AST (module Language.$language.AST) where
              -- Language definition for $language, enerated by ast-generate. Do not edit!
              import Prelude ()
                      |]

  let programText = T.unlines [header, modheader, T.pack (pprint decls)]
  hasOrmolu <- findExecutable "ormolu"
  if isNothing hasOrmolu
    then T.putStrLn programText
    else do
      (path, tf) <- openTempFile "/tmp" "generated.hs"
      print path
      T.hPutStrLn tf programText
      hClose tf
      callProcess "sed" ["-i", "-e", "s/AST.Traversable1.Class.Traversable1 someConstraint/(AST.Traversable1.Class.Traversable1 someConstraint)/g", path]
      callProcess "ormolu" ["--mode", "inplace", path]
      readFile path >>= putStrLn

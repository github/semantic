{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import AST.GenerateSyntax
import GHC.Generics (Generic)
import Language.Haskell.TH
import qualified Options.Generic as Opt
import System.Directory
import qualified TreeSitter.JSON as JSON (tree_sitter_json)

data Config = Config
  { language :: String,
    path :: FilePath
  }
  deriving (Show, Generic)

instance Opt.ParseRecord Config

main :: IO ()
main = do
  args <- Opt.getRecord "generate-ast"
  print @Config args
  absolute <- makeAbsolute (path args)
  decls <- runQ (astDeclarationsRelative JSON.tree_sitter_json absolute)
  putStrLn (pprint decls)

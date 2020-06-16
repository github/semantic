{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Language.Rust.AST
( module Language.Rust.AST
, Rust.getTestCorpusDir
) where

import           AST.GenerateSyntax
import           AST.Token
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.Rust as Rust (getNodeTypesPath, getTestCorpusDir, tree_sitter_rust)

runIO Rust.getNodeTypesPath >>= astDeclarationsForLanguage Rust.tree_sitter_rust

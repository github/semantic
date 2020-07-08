{-# LANGUAGE CPP #-}
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
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Rust.AST
( module Language.Rust.AST
, Rust.getTestCorpusDir
) where

import           AST.GenerateSyntax
import           AST.Token
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.Rust as Rust (getNodeTypesPath, getTestCorpusDir, tree_sitter_rust)

#ifdef NODE_TYPES_PATH
astDeclarationsForLanguage Rust.tree_sitter_rust NODE_TYPES_PATH
#else
runIO Rust.getNodeTypesPath >>= astDeclarationsForLanguage Rust.tree_sitter_rust
#endif

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
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Language.OCaml.AST
( module Language.OCaml.AST
, OCaml.getTestCorpusDir
) where

import           Prelude hiding (False, Float, Integer, String, True, Functor)
import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.OCaml as OCaml (getNodeTypesPath, getTestCorpusDir, tree_sitter_ocaml)

#ifdef NODE_TYPES_PATH
astDeclarationsForLanguage OCaml.tree_sitter_ocaml
#else
runIO OCaml.getNodeTypesPath >>= astDeclarationsForLanguage OCaml.tree_sitter_ocaml
#endif

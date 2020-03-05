{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.QL.AST
( module Language.QL.AST
, QL.getTestCorpusDir
) where

import           AST.GenerateSyntax
import           AST.Token()
import           Language.Haskell.TH.Syntax (runIO)
import           Prelude hiding (Bool, Eq, Float, Integer, String)
import qualified TreeSitter.QL as QL (getNodeTypesPath, getTestCorpusDir, tree_sitter_ql)

runIO QL.getNodeTypesPath >>= astDeclarationsForLanguage QL.tree_sitter_ql

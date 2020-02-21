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
module Language.JSON.AST
( module Language.JSON.AST
, JSON.getTestCorpusDir
) where

import           Prelude hiding (String)
import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.JSON as JSON (getNodeTypesPath, getTestCorpusDir, tree_sitter_json)

runIO JSON.getNodeTypesPath >>= astDeclarationsForLanguage JSON.tree_sitter_json

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

module Language.TypeScript.AST
( module Language.TypeScript.AST
, TypeScript.getTestCorpusDir
) where

import           Prelude hiding (False, Float, Integer, String, True)
import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.TypeScript as TypeScript (getNodeTypesPath, getTestCorpusDir, tree_sitter_typescript)

runIO TypeScript.getNodeTypesPath >>= astDeclarationsForLanguage TypeScript.tree_sitter_typescript

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

module Language.Python.AST
( module Language.Python.AST
) where

import           Prelude hiding (False, Float, Integer, String, True)
import           AST.GenerateSyntax
import qualified Language.Python.Grammar as Grammar
import TreeSitter.Python (getNodeTypesPath, tree_sitter_python)

runIO TypeScript.getNodeTypesPath >>= astDeclarationsForLanguage Grammar.tree_sitter_python
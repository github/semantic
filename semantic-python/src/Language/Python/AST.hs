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
import           TreeSitter.GenerateSyntax
import qualified TreeSitter.Python as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_python "../../../vendor/tree-sitter-python/src/node-types.json"

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

module Language.Java.AST
( module Language.Java.AST
) where

import           TreeSitter.GenerateSyntax
import qualified TreeSitter.Java as Grammar
import           TreeSitter.Token

astDeclarationsForLanguage Grammar.tree_sitter_java "../../vendor/tree-sitter-java/src/node-types.json"
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

import           AST.GenerateSyntax
import           AST.Token
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.Java as Java

runIO Java.getNodeTypesPath >>= astDeclarationsForLanguage Java.tree_sitter_java

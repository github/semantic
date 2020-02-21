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

module Language.PHP.AST
( module Language.PHP.AST
) where

import           AST.GenerateSyntax
import           AST.Token()
import           Language.Haskell.TH.Syntax (runIO)
import           Prelude hiding (String, Integer, Float)
import qualified TreeSitter.PHP as PHP (getNodeTypesPath, tree_sitter_php)

runIO PHP.getNodeTypesPath >>= astDeclarationsForLanguage PHP.tree_sitter_php

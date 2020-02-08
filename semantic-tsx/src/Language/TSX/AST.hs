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

module Language.TSX.AST
( module Language.TSX.AST
) where

import           Prelude hiding (False, Float, Integer, String, True)
import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.TSX (getNodeTypesPath, tree_sitter_tsx)

runIO TreeSitter.TSX.getNodeTypesPath >>= astDeclarationsForLanguage TreeSitter.TSX.tree_sitter_tsx

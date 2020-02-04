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
import qualified TreeSitter.TSX as TSX

runIO TSX.getNodeTypesPath >>= astDeclarationsForLanguage TSX.tree_sitter_tsx

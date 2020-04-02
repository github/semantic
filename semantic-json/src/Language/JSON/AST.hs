{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.JSON.AST
( module Language.JSON.AST
, JSON.getTestCorpusDir
) where

import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import           Prelude hiding (String)
import qualified TreeSitter.JSON as JSON (getNodeTypesPath, getTestCorpusDir, tree_sitter_json)

runIO JSON.getNodeTypesPath >>= astDeclarationsForLanguage JSON.tree_sitter_json

{-# LANGUAGE CPP #-}
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
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Language.JSON.AST
( module Language.JSON.AST
, JSON.getTestCorpusDir
) where

import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import           Prelude hiding (String)
import qualified TreeSitter.JSON as JSON (getNodeTypesPath, getTestCorpusDir, tree_sitter_json)

#ifdef NODE_TYPES_PATH
astDeclarationsForLanguage JSON.tree_sitter_json NODE_TYPES_PATH
#else
runIO JSON.getNodeTypesPath >>= astDeclarationsForLanguage JSON.tree_sitter_json
#endif

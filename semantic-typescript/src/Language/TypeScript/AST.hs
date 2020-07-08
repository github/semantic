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
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Language.TypeScript.AST
( module Language.TypeScript.AST
, TypeScript.getTestCorpusDir
) where

import           Prelude hiding (False, Float, Integer, String, True)
import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.TypeScript as TypeScript (getNodeTypesPath, getTestCorpusDir, tree_sitter_typescript)

#ifdef NODE_TYPES_PATH
astDeclarationsForLanguage TypeScript.tree_sitter_typescript NODE_TYPES_PATH
#else
runIO TypeScript.getNodeTypesPath >>= astDeclarationsForLanguage TypeScript.tree_sitter_typescript
#endif

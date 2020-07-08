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

module Language.CodeQL.AST
( module Language.CodeQL.AST
, CodeQL.getTestCorpusDir
) where

import           AST.GenerateSyntax
import           AST.Token()
import           Language.Haskell.TH.Syntax (runIO)
import           Prelude hiding (Bool, Eq, Float, Integer, String)
import qualified TreeSitter.QL as CodeQL (getNodeTypesPath, getTestCorpusDir, tree_sitter_ql)

#ifdef NODE_TYPES_PATH
astDeclarationsForLanguage CodeQL.tree_sitter_ql NODE_TYPES_PATH
#else
runIO CodeQL.getNodeTypesPath >>= astDeclarationsForLanguage CodeQL.tree_sitter_ql
#endif

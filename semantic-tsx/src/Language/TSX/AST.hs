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

module Language.TSX.AST
( module Language.TSX.AST
, TSX.getTestCorpusDir
) where

import           Prelude hiding (False, Float, Integer, String, True)
import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.TSX as TSX (getNodeTypesPath, getTestCorpusDir, tree_sitter_tsx)

#ifdef NODE_TYPES_PATH
astDeclarationsForLanguage TSX.tree_sitter_tsx NODE_TYPES_PATH
#else
runIO TSX.getNodeTypesPath >>= astDeclarationsForLanguage TSX.tree_sitter_tsx
#endif

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

module Language.Go.AST
( module Language.Go.AST
, Go.getTestCorpusDir
) where

import           Prelude hiding (False, Float, Integer, Rational, String, True)
import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.Go as Go (getNodeTypesPath, getTestCorpusDir, tree_sitter_go)

#ifdef NODE_TYPES_PATH
astDeclarationsForLanguage Go.tree_sitter_go NODE_TYPES_PATH
#else
runIO Go.getNodeTypesPath >>= astDeclarationsForLanguage Go.tree_sitter_go
#endif

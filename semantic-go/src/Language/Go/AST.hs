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

module Language.Go.AST
( module Language.Go.AST
) where

import           Prelude hiding (False, Float, Integer, Rational, String, True)
import           AST.GenerateSyntax
import qualified TreeSitter.Go as Go

runIO Go.getNodeTypesPath >>= astDeclarationsForLanguage Go.tree_sitter_go

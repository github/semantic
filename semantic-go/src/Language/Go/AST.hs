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
import           TreeSitter.GenerateSyntax
import qualified TreeSitter.Go as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_go "../../vendor/tree-sitter-go/src/node-types.json"
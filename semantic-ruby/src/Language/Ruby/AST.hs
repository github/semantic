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

module Language.Ruby.AST
( module Language.Ruby.AST
) where

import           Prelude hiding (False, Float, Integer, Rational, String, True)
import           AST.GenerateSyntax
import qualified Language.Ruby.Grammar as Grammar

astDeclarationsForLanguage Grammar.tree_sitter_ruby "../../../vendor/tree-sitter-ruby/src/node-types.json"

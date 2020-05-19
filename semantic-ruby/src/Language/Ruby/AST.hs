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
, Ruby.getTestCorpusDir
) where

import           Prelude hiding (False, Float, Integer, Rational, String, True)
import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.Ruby as Ruby (getNodeTypesPath, getTestCorpusDir, tree_sitter_ruby)

runIO Ruby.getNodeTypesPath >>= astDeclarationsForLanguage Ruby.tree_sitter_ruby

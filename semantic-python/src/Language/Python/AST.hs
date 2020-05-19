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

module Language.Python.AST
( module Language.Python.AST
, Python.getTestCorpusDir
) where

import           Prelude hiding (False, Float, Integer, String, True)
import           AST.GenerateSyntax
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.Python as Python (getNodeTypesPath, getTestCorpusDir, tree_sitter_python)

astDeclarationsForLanguage Python.tree_sitter_python "/private/var/tmp/_bazel_patrickt/9ab2276bd7abbc23224fda87a0d88539/external/stackage/tree-sitter-python-0.9.0.2/vendor/tree-sitter-python/src/node-types.json"

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

module Language.Python.AST
  ( module Language.Python.AST,
    Python.getTestCorpusDir,
  )
where

import AST.GenerateSyntax
import Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.Python as Python (getNodeTypesPath, getTestCorpusDir, tree_sitter_python)
import Prelude hiding (False, Float, Integer, String, True)

runIO Python.getNodeTypesPath >>= astDeclarationsForLanguage Python.tree_sitter_python

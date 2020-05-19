{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Python.Patterns
  ( pattern SingleIdentifier
  ) where

import           AST.Element
import qualified AST.Parse as Parse
import qualified Analysis.Name
import qualified Language.Python.AST as Py

-- | Useful pattern synonym for extracting a single identifier from
-- a Python ExpressionList. Easier than pattern-matching every time.
-- TODO: when this is finished, we won't need this pattern, as we'll
-- handle ExpressionLists the smart way every time.
pattern SingleIdentifier :: Analysis.Name.Name -> Py.ExpressionList a
pattern SingleIdentifier n <- Py.ExpressionList
  { Py.extraChildren =
    [ Parse.Success (Py.Expression (Prj (Py.PrimaryExpression (Prj Py.Identifier { text = Analysis.Name.name -> n }))))
    ]
  }

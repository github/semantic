{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Python.Patterns
  ( pattern SingleIdentifier
  ) where

import           AST.Element
import           Data.Coerce
import           Data.Text (Text)
import qualified TreeSitter.Python.AST as Py

-- | Useful pattern synonym for extracting a single identifier from
-- a Python ExpressionList. Easier than pattern-matching every time.
-- TODO: when this is finished, we won't need this pattern, as we'll
-- handle ExpressionLists the smart way every time.
pattern SingleIdentifier :: Coercible t Text => t -> Py.ExpressionList a
pattern SingleIdentifier name <- Py.ExpressionList
  { Py.extraChildren =
    [ Py.Expression (Prj (Py.PrimaryExpression (Prj Py.Identifier { text = coerce -> name })))
    ]
  }

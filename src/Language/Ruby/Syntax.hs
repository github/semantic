{-# LANGUAGE DataKinds #-}
module Language.Ruby.Syntax where

import Data.Functor.Union
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement

type Ruby = Union
  '[Comment.Comment
  , Declaration.Class
  , Declaration.Method
  , Literal.Boolean
  , Statement.If
  , Statement.Return
  , Statement.Yield
  ]

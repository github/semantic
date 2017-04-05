{-# LANGUAGE DataKinds, GADTs #-}
module Language.Ruby.Syntax where

import Control.Monad.Free.Freer
import Data.Functor.Union
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Prologue

-- | The type of Ruby syntax.
type Syntax = Union
  '[Comment.Comment
  , Declaration.Class
  , Declaration.Method
  , Literal.Boolean
  , Statement.If
  , Statement.Return
  , Statement.Yield
  ]

-- | Parse a node with the specified symbol. Produces a list of subnodes.
rule :: symbol -> Assignment symbol [a]
rule sym = Rule sym `Then` return


data AssignmentF symbol a where
  Rule :: symbol -> AssignmentF symbol [a]

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment symbol = Freer (AssignmentF symbol)


-- | A program in some syntax functor, over which we can perform analyses.
type Program = Freer


-- | Statically-known rules corresponding to symbols in the grammar.
data Grammar = Program | Uninterpreted

-- | Assignment onto a program in Ruby’s syntax.
assignment :: Assignment Grammar (Program Syntax (Maybe ()))
assignment = foldr (>>) (return Nothing) <$> rule Program

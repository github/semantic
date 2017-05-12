{-# LANGUAGE DataKinds, TemplateHaskell #-}
module Language.Python.Syntax where

import Data.Functor.Union
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Language.Haskell.TH hiding (location)
import Prologue hiding (Location)
import Term
import Text.Parser.TreeSitter.Python
import Text.Parser.TreeSitter.Language

-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_python

type Syntax = Union Syntax'
type Syntax' =
  '[Comment.Comment]

-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment (Node Grammar) [Term Syntax Location]
assignment = symbol Module *> children (many comment)

comment :: Assignment (Node Grammar) (Term Syntax Location)
comment = makeTerm <$ symbol Comment <*> location <*> (Comment.Comment <$> source)

makeTerm :: InUnion Syntax' f => a -> f (Term Syntax a) -> Term Syntax a
makeTerm a f = cofree (a :< inj f)

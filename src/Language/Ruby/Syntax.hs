{-# LANGUAGE DataKinds, TemplateHaskell #-}
module Language.Ruby.Syntax where

import Control.Monad.Free.Freer hiding (Return)
import Data.Functor.Union
import Data.Syntax.Assignment
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Language.Haskell.TH
import Prologue
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.Ruby

-- | The type of Ruby syntax.
type Syntax = Union
  '[Comment.Comment
  , Declaration.Class
  , Declaration.Method
  , Literal.Array
  , Literal.Boolean
  , Literal.Hash
  , Literal.Integer
  , Literal.String
  , Literal.Symbol
  , Statement.If
  , Statement.Return
  , Statement.Yield
  , Syntax.Empty
  , Syntax.Identifier
  ]


-- | A program in some syntax functor, over which we can perform analyses.
type Program = Freer Syntax


-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment Grammar [Program (Maybe a)]
assignment = rule Program *> children (many declaration)

declaration :: Assignment Grammar (Program (Maybe a))
declaration = comment <|> class' <|> method

class' :: Assignment Grammar (Program (Maybe a))
class' = wrapU <$  rule Class
               <*> children (Declaration.Class <$> constant <*> pure [] <*> many declaration)

constant :: Assignment Grammar (Program a)
constant = wrapU . Syntax.Identifier <$ rule Constant <*> content

identifier :: Assignment Grammar (Program a)
identifier = wrapU . Syntax.Identifier <$ rule Identifier <*> content

method :: Assignment Grammar (Program (Maybe a))
method = wrapU <$  rule Method
               <*> children (Declaration.Method <$> identifier <*> pure [] <*> (statement <|> pure (wrapU Syntax.Empty)))

statement :: Assignment Grammar (Program a)
statement  =  rule Return *> children (wrapU . Statement.Return <$> expr <|> pure (wrapU Syntax.Empty))
          <|> rule Yield *> children (wrapU . Statement.Yield <$> expr <|> pure (wrapU Syntax.Empty))
          <|> expr

comment :: Assignment Grammar (Program a)
comment = wrapU . Comment.Comment <$ rule Comment <*> content

if' :: Assignment Grammar (Program a)
if' = wrapU <$ rule If <*> children (Statement.If <$> expr <*> expr <*> expr)

expr :: Assignment Grammar (Program a)
expr = if' <|> literal

literal :: Assignment Grammar (Program a)
literal  =  wrapU (Literal.Boolean Prologue.True) <$ rule Language.Ruby.Syntax.True <* content
        <|> wrapU (Literal.Boolean Prologue.False) <$ rule Language.Ruby.Syntax.False <* content

{-# LANGUAGE DataKinds, TemplateHaskell #-}
module Language.Ruby.Syntax where

import Data.Functor.Union
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Language.Haskell.TH
import Prologue hiding (optional)
import Term
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.Ruby

-- | The type of Ruby syntax.
type Syntax = Union Syntax'
type Syntax' =
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
  , Syntax.Identifier
  , []
  ]


term :: InUnion Syntax' f => () -> f (Term Syntax ()) -> Term Syntax ()
term a f = cofree $ a :< inj f


-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment Grammar [Term Syntax ()]
assignment = rule Program *> children (many declaration)

declaration :: Assignment Grammar (Term Syntax ())
declaration = comment <|> class' <|> method

class' :: Assignment Grammar (Term Syntax ())
class' = term () <$  rule Class
                 <*> children (Declaration.Class <$> constant <*> pure [] <*> many declaration)

constant :: Assignment Grammar (Term Syntax ())
constant = term () . Syntax.Identifier <$ rule Constant <*> content

identifier :: Assignment Grammar (Term Syntax ())
identifier = term () . Syntax.Identifier <$ rule Identifier <*> content

method :: Assignment Grammar (Term Syntax ())
method = term () <$  rule Method
                 <*> children (Declaration.Method <$> identifier <*> pure [] <*> (term () <$> many statement))

statement :: Assignment Grammar (Term Syntax ())
statement  =  term () . Statement.Return <$ rule Return <*> children (optional expr)
          <|> term () . Statement.Yield <$ rule Yield <*> children (optional expr)
          <|> expr

comment :: Assignment Grammar (Term Syntax ())
comment = term () . Comment.Comment <$ rule Comment <*> content

if' :: Assignment Grammar (Term Syntax ())
if' = go If
  where go symbol = term () <$ rule symbol <*> children (Statement.If <$> statement <*> (term () <$> many statement) <*> (go Elsif <|> term () <$ rule Else <*> children (many statement)))

expr :: Assignment Grammar (Term Syntax ())
expr = if' <|> literal

literal :: Assignment Grammar (Term Syntax ())
literal  =  term () Literal.true <$ rule Language.Ruby.Syntax.True <* content
        <|> term () Literal.false <$ rule Language.Ruby.Syntax.False <* content

optional :: Assignment Grammar (Term Syntax ()) -> Assignment Grammar (Term Syntax ())
optional a = a <|> pure (() `term` [])

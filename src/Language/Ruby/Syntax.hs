{-# LANGUAGE DataKinds, TemplateHaskell, TypeOperators #-}
module Language.Ruby.Syntax where

import Data.Functor.Union
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Language.Haskell.TH
import Prologue hiding (optional, unless, get)
import Term
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.Ruby

-- | The type of Ruby syntax.
type Syntax = Union Syntax'
type Syntax' =
  '[Comment.Comment
  , Declaration.Class
  , Declaration.Method
  , Expression.Not
  , Literal.Array
  , Literal.Boolean
  , Literal.Hash
  , Literal.Integer
  , Literal.String
  , Literal.Symbol
  , Statement.Break
  , Statement.Continue
  , Statement.If
  , Statement.Return
  , Statement.Yield
  , Syntax.Empty
  , Syntax.Identifier
  , []
  ]


-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment (Record (Grammar ': fs)) [Term Syntax (Record fs)]
assignment = symbol Program *> children (many declaration)

declaration :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
declaration = comment <|> class' <|> method

class' :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
class' = term <*  symbol Class
              <*> children (Declaration.Class <$> (constant <|> scopeResolution) <*> (superclass <|> pure []) <*> many declaration)
  where superclass = pure <$ symbol Superclass <*> children constant
        scopeResolution = symbol ScopeResolution *> children (constant <|> identifier)

constant :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
constant = term <*> (Syntax.Identifier <$ symbol Constant <*> source)

identifier :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
identifier = term <*> (Syntax.Identifier <$ symbol Identifier <*> source)

method :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
method = term <*  symbol Method
              <*> children (Declaration.Method <$> identifier <*> pure [] <*> (term <*> many statement))

statement :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
statement  =  exit Statement.Return Return
          <|> exit Statement.Yield Yield
          <|> exit Statement.Break Break
          <|> exit Statement.Continue Next
          <|> if'
          <|> ifModifier
          <|> unless
          <|> unlessModifier
          <|> literal
  where exit construct sym = term <*> (construct <$ symbol sym <*> children (optional (symbol ArgumentList *> children statement)))

comment :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
comment = term <*> (Comment.Comment <$ symbol Comment <*> source)

if' :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
if' = go If
  where go s = term <* symbol s <*> children (Statement.If <$> statement <*> (term <*> many statement) <*> optional (go Elsif <|> term <* symbol Else <*> children (many statement)))

ifModifier :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
ifModifier = term <* symbol IfModifier <*> children (flip Statement.If <$> statement <*> statement <*> (term <*> pure Syntax.Empty))

unless :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
unless = term <* symbol Unless <*> children (Statement.If <$> (term <*> (Expression.Not <$> statement)) <*> (term <*> many statement) <*> optional (term <* symbol Else <*> children (many statement)))

unlessModifier :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
unlessModifier = term <* symbol UnlessModifier <*> children (flip Statement.If <$> statement <*> (term <*> (Expression.Not <$> statement)) <*> (term <*> pure Syntax.Empty))

literal :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
literal  =  term <*> (Literal.true <$ symbol Language.Ruby.Syntax.True <* source)
        <|> term <*> (Literal.false <$ symbol Language.Ruby.Syntax.False <* source)
        <|> term <*> (Literal.Integer <$ symbol Language.Ruby.Syntax.Integer <*> source)

-- | Assignment of the current node’s annotation.
term :: InUnion Syntax' f => Assignment (Record (h ': t)) (f (Term Syntax (Record t)) -> Term Syntax (Record t))
term = (\ a f -> cofree $ rtail a :< inj f) <$> get

optional :: Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs)) -> Assignment (Record (Grammar ': fs)) (Term Syntax (Record fs))
optional a = a <|> term <*> pure Syntax.Empty

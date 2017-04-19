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
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.Ruby

-- | The type of Ruby syntax.
type Syntax =
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


-- | A program in some syntax functor, over which we can perform analyses.
type Program = Cofree (Union Syntax) ()

term :: InUnion Syntax f => () -> f Program -> Program
term a f = cofree $ a :< inj f


-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment Grammar [Program]
assignment = rule Program *> children (many declaration)

declaration :: Assignment Grammar Program
declaration = comment <|> class' <|> method

class' :: Assignment Grammar Program
class' = term () <$  rule Class
                 <*> children (Declaration.Class <$> constant <*> pure [] <*> many declaration)

constant :: Assignment Grammar Program
constant = term () . Syntax.Identifier <$ rule Constant <*> content

identifier :: Assignment Grammar Program
identifier = term () . Syntax.Identifier <$ rule Identifier <*> content

method :: Assignment Grammar Program
method = term () <$  rule Method
                 <*> children (Declaration.Method <$> identifier <*> pure [] <*> (term () <$> many statement))

statement :: Assignment Grammar Program
statement  =  term () . Statement.Return <$ rule Return <*> children (optional expr)
          <|> term () . Statement.Yield <$ rule Yield <*> children (optional expr)
          <|> expr

comment :: Assignment Grammar Program
comment = term () . Comment.Comment <$ rule Comment <*> content

if' :: Assignment Grammar Program
if' = go If
  where go symbol = term () <$ rule symbol <*> children (Statement.If <$> statement <*> (term () <$> many statement) <*> (go Elsif <|> term () <$ rule Else <*> children (many statement)))

expr :: Assignment Grammar Program
expr = if' <|> literal

literal :: Assignment Grammar Program
literal  =  term () Literal.true <$ rule Language.Ruby.Syntax.True <* content
        <|> term () Literal.false <$ rule Language.Ruby.Syntax.False <* content

optional :: Assignment Grammar Program -> Assignment Grammar Program
optional a = a <|> pure (() `term` [])

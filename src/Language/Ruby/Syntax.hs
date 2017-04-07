{-# LANGUAGE DataKinds #-}
module Language.Ruby.Syntax where

import Control.Monad.Free.Freer
import Data.Functor.Union
import Data.Syntax.Assignment
import qualified Data.Syntax as Syntax
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
  , Syntax.Identifier
  ]


-- | A program in some syntax functor, over which we can perform analyses.
type Program = Freer


-- | Statically-known rules corresponding to symbols in the grammar.
data Grammar = Program | Uninterpreted | BeginBlock | EndBlock | Undef | Alias | Comment | True' | False' | Return | Yield | Break | Next | Redo | Retry | IfModifier | UnlessModifier | WhileModifier | UntilModifier | RescueModifier | While | Until | For | Do | Case | When | Pattern | If | Unless | Elsif | Else | Begin | Ensure | Rescue | Exceptions | ExceptionVariable | ElementReference | ScopeResolution | Call | MethodCall | ArgumentList | ArgumentListWithParens | SplatArgument | HashSplatArgument | BlockArgument | Class | Constant | Method | Identifier
  deriving (Enum, Eq, Ord, Show)

-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment Grammar (Program Syntax (Maybe a))
assignment = foldr (>>) (pure Nothing) <$> rule Program (children (many declaration))
  where declaration = comment <|> class' <|> method
        class' = rule Class (wrapU <$> (Declaration.Class <$> constant <*> pure [] <*> declaration))
        constant = rule Constant (wrapU <$> (Syntax.Identifier <$> content))
        identifier = rule Identifier (wrapU <$> (Syntax.Identifier <$> content))
        method = rule Method (wrapU <$> (Declaration.Method <$> identifier <*> pure [] <*> statement))
        statement = expr

comment :: Assignment Grammar (Program Syntax a)
comment = wrapU . Comment.Comment <$> (rule Comment content)

if' :: Assignment Grammar (Program Syntax a)
if' = rule If (wrapU <$> (Statement.If <$> expr <*> expr <*> expr))

expr :: Assignment Grammar (Program Syntax a)
expr = if'

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

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment symbol = Freer (AssignmentF symbol)

data AssignmentF symbol a where
  Rule :: symbol -> AssignmentF symbol a
  Content :: AssignmentF symbol ByteString
  Children :: AssignmentF symbol [a]
  Child :: AssignmentF symbol a
  And :: a -> a -> AssignmentF symbol a

rule :: symbol -> Assignment symbol a
rule symbol = Rule symbol `Then` return

content :: Assignment symbol ByteString
content = Content `Then` return

children :: Assignment symbol [a]
children = Children `Then` return

child :: Assignment symbol a
child = Child `Then` return


-- | A program in some syntax functor, over which we can perform analyses.
type Program = Freer


-- | Statically-known rules corresponding to symbols in the grammar.
data Grammar = Program | Uninterpreted | BeginBlock | EndBlock | Undef | Alias | Comment | True' | False' | If
  deriving (Enum, Eq, Ord, Show)

-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment Grammar (Program Syntax (Maybe ()))
assignment = foldr (>>) (return Nothing) <$> rule Program
comment :: Assignment Grammar (Program Syntax a)
comment = wrapU . Comment.Comment <$> (rule Comment <> content)

if' :: Assignment Grammar (Program Syntax a)
if' = rule If <> (wrapU <$> (Statement.If <$> child <*> child <*> child))


instance Semigroup (Assignment symbol a) where
  a <> b = And a b `Then` identity

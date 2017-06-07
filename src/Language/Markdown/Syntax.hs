{-# LANGUAGE DataKinds, DeriveAnyClass #-}
module Language.Markdown.Syntax
( assignment
, Syntax
, Grammar
, Error
, Term
) where

import qualified CMark
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import Data.Functor.Union
import Data.Record
import Data.Syntax.Assignment hiding (Assignment, Error)
import GHC.Generics
import GHC.Stack
import qualified Data.Syntax.Assignment as Assignment
import Prologue hiding (Location)
import qualified Term
import Text.Parser.TreeSitter.Language (Symbol(..), SymbolType(..))

data Grammar
  = Document'
  | Paragraph'
  | Heading
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Symbol Grammar where
  symbolType _ = Regular

type Syntax =
  '[ Document
   , Paragraph
   ]

newtype Document a = Document [a]
 deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Document where liftEq = genericLiftEq
instance Show1 Document where liftShowsPrec = genericLiftShowsPrec

newtype Paragraph a = Paragraph [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Paragraph where liftEq = genericLiftEq
instance Show1 Paragraph where liftShowsPrec = genericLiftShowsPrec


type Error = Assignment.Error Grammar
type Term = Term.Term (Union Syntax) (Record Location)
type Assignment = Assignment.Assignment CMark.NodeType Grammar Term

assignment :: HasCallStack => Assignment
assignment = empty

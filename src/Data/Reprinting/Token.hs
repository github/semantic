module Data.Reprinting.Token
  ( Token (..)
  , Element (..)
  , Control (..)
  , Context (..)
  , Operator (..)
  ) where

import Data.Text (Text)
import Data.Source (Source)


-- language agnostic -> CToken -> SToken -> language specific -> formatting whitespace
-- tokenize          -> CToken -> SToken -> translate         -> typeset
-- Seq Token         -> CToken -> SToken -> Seq Splice        -> Doc

-- | 'Token' encapsulates 'Element' and 'Control' tokens, as well as sliced
-- portions of the original 'Source' for a given AST.
data Token
  = Chunk Source     -- original 'Source' from AST, unmodified by pipeline.
  | TElement Element --
  | TControl Control
    deriving (Show, Eq)

-- | 'Element' tokens describe atomic pieces of source code to be
-- output to a rendered document. These tokens are language-agnostic
-- and are interpreted into language-specific representations at a
-- later point in the reprinting pipeline.
data Element
  = Fragment Text -- ^ A literal chunk of text.
  | Truth Bool    -- ^ A boolean value.
  | Nullity       -- ^ @null@ or @nil@ or some other zero value.
  | Separator     -- ^ Some sort of delimiter, interpreted in some 'Context'.
  | Open          -- ^ The beginning of some 'Context', such as an @[@ or @{@.
  | Close         -- ^ The opposite of 'Open'.
    deriving (Eq, Show)

-- | 'Control' tokens describe information about some AST's context.
-- Though these are ultimately rendered as whitespace (or nothing) on
-- the page, they are needed to provide information as to how deeply
-- subsequent entries in the pipeline should indent.
data Control
  = Enter Context
  | Exit Context
  | Log String
    deriving (Eq, Show)

-- | A 'Context' represents a scope in which other tokens can be
-- interpreted. For example, in the 'Imperative' context a 'Separator'
-- could be a semicolon or newline, whereas in a 'List' context a
-- 'Separator' is probably going to be a comma.
data Context
  = List
  | Associative
  | Pair
  | Infix Operator
  | Imperative
  | TopLevel
    deriving (Show, Eq)

-- | A sum type representing every concievable infix operator a
-- language can define. These are handled by instances of 'Concrete'
-- and given appropriate precedence.
data Operator
  = Add
    deriving (Show, Eq)

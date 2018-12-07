module Data.Reprinting.Scope
  ( Scope (..)
  , precedenceOf
  , imperativeDepth
  ) where

import Data.Reprinting.Operator


-- | A 'Scope' represents a scope in which other tokens can be
-- interpreted. For example, in the 'Imperative' context a 'Sep'
-- could be a semicolon or newline, whereas in a 'List' context a
-- 'Sep' is probably going to be a comma.
-- TODO: look into sharing control-flow constructs with 'Flow'
data Scope
  = List                -- ^ List literals (usually comma-separated, in square brackets)
  | Hash                -- ^ Hashes (key-value pairs, in curly brackets)
  | Pair                -- ^ Colon-separated key-value pairs
  | Slice               -- ^ Range-selection context, as in Go or Python
  | Method              -- ^ Member-function declaration
  | Atom                -- ^ Quoted symbols, e.g. Ruby Symbol
  | Function            -- ^ Function declaration
  | Namespace           -- ^ Namespace/module context
  | Call                -- ^ Function call (usually comma-separated arguments)
  | Params              -- ^ Function parameters (ibid.)
  | Return              -- ^ Zero or more values
  | Loop                -- ^ @for@, @while@, @foreach@ loops
  | If                  -- ^ Conditionals
  | Case                -- ^ @case@ or @switch@ context
  | InfixL Operator Int -- ^ Left-associative operators, with context
  | Prefix Operator     -- ^ Prefix operators
  | Indexing            -- ^ Single-element array/list indexing
  | Imperative          -- ^ ALGOL-style top-to-bottom int
  | Interpolation       -- ^ String interpolation
  | Catch               -- ^ @try@
  | Finally             -- ^ @except@
  | BeginBlock          -- ^ Ruby-specific: @BEGIN@
  | EndBlock            -- ^ Ruby-specific: @END@
  | Class               -- ^ Class definition
    deriving (Show, Eq)

precedenceOf :: [Scope] -> Int
precedenceOf cs = case filter isInfix cs of
  (InfixL _ n:_) -> n
  _ -> 0
  where isInfix (InfixL _ _)  = True
        isInfix _             = False


-- | Depth of imperative scope.
imperativeDepth :: [Scope] -> Int
imperativeDepth = length . filter (== Imperative)

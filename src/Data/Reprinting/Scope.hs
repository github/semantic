module Data.Reprinting.Scope
  ( Scope (..)
  , precedenceOf
  , imperativeDepth
  ) where

import Data.Reprinting.Operator

-- | A 'Scope' represents a scope in which other tokens can be
-- interpreted. For example, in the 'Imperative' context a 'TSep'
-- could be a semicolon or newline, whereas in a 'List' context a
-- 'TSep' is probably going to be a comma.
data Scope
  = List
  | Hash
  | Pair
  | Method
  | Function
  | Call
  | Params
  | Return
  | If
  | InfixL Operator Int
  | Imperative
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

module Core.Expr
( Expr(..)
, id'
, const'
, true
, false
, not'
, and'
, or'
, ($$)
) where

import Membrane.Syntax

-- | A lambda calculus with integers, conditionals, and exceptions.
data Expr
  = Var Name
  | Abs Name Expr
  | App Expr Expr
  | Let Name Expr Expr
  | Lit Int
  | Op1 Op1 Expr
  | If0 Expr Expr Expr
  | Die String
  deriving (Eq, Ord, Show)


id' :: Expr
id' = Abs "x" (Var "x")

const' :: Expr
const' = Abs "a" (Abs "b" (Var "a"))


true, false :: Expr
true  = Abs "a" (Abs "b" (Var "a"))
false = Abs "a" (Abs "b" (Var "b"))

not' :: Expr
not' = Abs "x" (Var "x" $$ false $$ true)

and', or' :: Expr
and' = Abs "a" (Abs "b" (Var "a" $$ true    $$ Var "b"))
or'  = Abs "a" (Abs "b" (Var "a" $$ Var "b" $$ false))


($$) :: Expr -> Expr -> Expr
($$) = App

infixl 9 $$

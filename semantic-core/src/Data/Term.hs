module Data.Term
( Term(..)
) where

data Term sig a
  = Var a
  | Term (sig (Term sig) a)

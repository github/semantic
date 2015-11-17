module Diff where

{-# LANGUAGE DeriveFunctor #-}

import Data.Maybe

data Free f a = Pure a | Roll (f (Free f a))
    deriving Functor
newtype Fix f = In { out :: f (Fix f) }
data Cofree f a = Unroll a (f (Cofree f a))
    deriving Functor

data Range = Range { start :: Int, end :: Int }

type Info = String
data AST a f
  = Leaf a
  | Keyed [(String, f)]
  | Indexed [f]
  deriving Functor

type Term a = Fix (AST a)
data Patch a = Patch { old :: Maybe a, new :: Maybe a }
type Diff a = Free (AST a) (Patch (Term a))

(</>) :: Maybe (Term a) -> Maybe (Term a) -> Diff a
(</>) a b = Pure $ Patch { old = a, new = b }

a :: Term String
a = In $ Keyed [
  ("hello", In $ Indexed [ In $ Leaf "hi" ]),
  ("goodbye", In $ Leaf "goodbye") ]

b :: Term String
b = In $ Keyed [
  ("hello", In $ Indexed []),
  ("goodbye", In $ Indexed []) ]

d :: Diff String
d = Roll $ Keyed [
  ("hello", Roll $ Indexed [ Just (In $ Leaf "hi") </> Nothing ]),
  ("goodbye", Just (In $ Leaf "goodbye") </> Just (In $ Indexed [])) ]

data Operation a f
  = Recur (Term a) (Term a) (Diff a -> f)
  | ByKey [(String, Term a)] [(String, Term a)] ([(String, Diff a)] -> f)
  | ByIndex [Term a] [Term a] ([Diff a] -> f)

type Algorithm a = Free (Operation a)

iter :: Functor f => (f a -> a) -> Free f a -> a
iter _ (Pure a) = a
iter phi (Roll m) = phi $ iter phi <$> m

cost :: Diff a -> Integer
cost f = iter c $ fmap g f where
    c (Leaf _) = 0
    c (Keyed xs) = sum $ snd <$> xs
    c (Indexed xs) = sum xs
    g _ = 1

-- interpret :: Algorithm a b -> b
-- interpret (Pure b) = b
-- interpret (Roll (Recur a b f)) = f $ Pure (Patch { old = Just (In a), new = Just (In b) })

type RangedTerm a = Cofree (AST a) Int
-- data Difff a f = Difff (Either (Patch (Term a)) (AST a f))
-- type RangedDiff a = Cofree (Difff a) Range
data AnnotatedAST a f = AnnotatedAST (Range, AST a f)
type RangedDiff a = Free (AnnotatedAST a) (Patch (Term a))

module Diff where

{-# LANGUAGE DeriveFunctor #-}

import Syntax
import Data.Maybe
import Data.Map
import Control.Monad.Free

newtype Fix f = In { out :: f (Fix f) }
data Cofree f a = Unroll a (f (Cofree f a))
    deriving Functor

data Range = Range { start :: Int, end :: Int }

type Info = String

type Term a = Fix (Syntax a)
data Patch a = Patch { old :: Maybe a, new :: Maybe a }
type Diff a = Free (Syntax a) (Patch (Term a))

(</>) :: Maybe (Term a) -> Maybe (Term a) -> Diff a
(</>) a b = Pure $ Patch { old = a, new = b }

a :: Term String
a = In $ Keyed $ fromList [
  ("hello", In $ Indexed [ In $ Leaf "hi" ]),
  ("goodbye", In $ Leaf "goodbye") ]

b :: Term String
b = In $ Keyed $ fromList [
  ("hello", In $ Indexed []),
  ("goodbye", In $ Indexed []) ]

d :: Diff String
d = Free $ Keyed $ fromList [
  ("hello", Free $ Indexed [ Just (In $ Leaf "hi") </> Nothing ]),
  ("goodbye", Just (In $ Leaf "goodbye") </> Just (In $ Indexed [])) ]

data Operation a f
  = Recur (Term a) (Term a) (Diff a -> f)
  | ByKey [(String, Term a)] [(String, Term a)] ([(String, Diff a)] -> f)
  | ByIndex [Term a] [Term a] ([Diff a] -> f)

type Algorithm a = Free (Operation a)

cost :: Diff a -> Integer
cost f = iter c $ fmap g f where
    c (Leaf _) = 0
    c (Keyed xs) = sum $ snd <$> xs
    c (Indexed xs) = sum xs
    c (Fixed xs) = sum xs
    g _ = 1

-- interpret :: Algorithm a b -> b
-- interpret (Pure b) = b
-- interpret (Free (Recur a b f)) = f $ Pure (Patch { old = Just (In a), new = Just (In b) })

type RangedTerm a = Cofree (Syntax a) Int
-- data Difff a f = Difff (Either (Patch (Term a)) (Syntax a f))
-- type RangedDiff a = Cofree (Difff a) Range
data AnnotatedSyntax a f = AnnotatedSyntax (Range, Syntax a f)
type RangedDiff a = Free (AnnotatedSyntax a) (Patch (Term a))

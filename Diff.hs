module Diff where

{-# LANGUAGE DeriveFunctor #-}

import Syntax
import Data.Maybe
import Data.Map
import Control.Monad.Free
import Data.Fix
import Control.Comonad.Cofree

data Range = Range { start :: Int, end :: Int }

data Info = Info -- Range [String]

type Term a = Fix (Syntax a)
data Patch a = Patch { old :: Maybe a, new :: Maybe a }
type Diff a = Free (Syntax a) (Patch (Term a))

(</>) :: Maybe (Term a) -> Maybe (Term a) -> Diff a
(</>) a b = Pure $ Patch { old = a, new = b }

a :: Term String
a = Fix $ Keyed $ fromList [
  ("hello", Fix $ Indexed [ Fix $ Leaf "hi" ]),
  ("goodbye", Fix $ Leaf "goodbye") ]

b :: Term String
b = Fix $ Keyed $ fromList [
  ("hello", Fix $ Indexed []),
  ("goodbye", Fix $ Indexed []) ]

d :: Diff String
d = Free $ Keyed $ fromList [
  ("hello", Free $ Indexed [ Just (Fix $ Leaf "hi") </> Nothing ]),
  ("goodbye", Just (Fix $ Leaf "goodbye") </> Just (Fix $ Indexed [])) ]

data Operation a f
  = Recur (Term a) (Term a) (Diff a -> f)
  | ByKey [(String, Term a)] [(String, Term a)] ([(String, Diff a)] -> f)
  | ByIndex [Term a] [Term a] ([Diff a] -> f)

type Algorithm a = Free (Operation a)

cost :: Diff a -> Integer
cost f = iter c $ fmap g f where
    c (Leaf _) = 0
    c (Keyed xs) = sum $ snd <$> (toList xs)
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

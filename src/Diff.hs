module Diff where

import Syntax
import Data.Maybe
import Data.Map
import Control.Monad.Free
import Data.Fix
import Control.Comonad.Cofree
import Patch

data Range = Range { start :: Int, end :: Int }

data Info = Info -- Range [String]

type Term a annotation = Cofree (Syntax a) annotation
type Diff a = Free (Syntax a) (Patch (Term a Info))

(</>) :: Maybe (Term a Info) -> Maybe (Term a Info) -> Diff a
(</>) a b = Pure Patch { old = a, new = b }

a :: Term String Info
a = Info :< (Keyed $ fromList [
  ("hello", Info :< Indexed [ Info :< Leaf "hi" ]),
  ("goodbye", Info :< Leaf "goodbye") ])

b :: Term String Info
b = Info :< (Keyed $ fromList [
  ("hello", Info :< Indexed []),
  ("goodbye", Info :< Indexed []) ])

d :: Diff String
d = Free $ Keyed $ fromList [
  ("hello", Free $ Indexed [ Just (Info :< Leaf "hi") </> Nothing ]),
  ("goodbye", Just (Info :< Leaf "goodbye") </> Just (Info :< Indexed [])) ]

cost :: Diff a -> Integer
cost f = iter c $ fmap g f where
  c (Leaf _) = 0
  c (Keyed xs) = sum $ snd <$> toList xs
  c (Indexed xs) = sum xs
  c (Fixed xs) = sum xs
  g _ = 1

type RangedTerm a = Cofree (Syntax a) Int
-- data Difff a f = Difff (Either (Patch (Term a)) (Syntax a f))
-- type RangedDiff a = Cofree (Difff a) Range
data AnnotatedSyntax a f = AnnotatedSyntax (Range, Syntax a f)
type RangedDiff a = Free (AnnotatedSyntax a) (Patch (Term a Info))

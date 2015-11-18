module Diff where

import Syntax
import Data.Maybe
import Data.Map
import Control.Monad.Free
import Control.Comonad.Cofree
import Patch
import Term

data Range = Range { start :: Int, end :: Int }

data Info = Info -- Range [String]
  deriving Eq

type Diff a = Free (Syntax a) (Patch (Term a Info))

cost :: Diff a -> Integer
cost f = iter c $ fmap g f where
  c (Leaf _) = 0
  c (Keyed xs) = sum $ snd <$> toList xs
  c (Indexed xs) = sum xs
  c (Fixed xs) = sum xs
  g _ = 1

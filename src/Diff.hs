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

type Diff a annotation = Free (Syntax a) (Patch (Term a annotation))

cost :: Diff a annotation -> Integer
cost f = iter c $ fmap (const 1) f where
  c (Leaf _) = 0
  c (Keyed xs) = sum $ snd <$> toList xs
  c (Indexed xs) = sum xs
  c (Fixed xs) = sum xs

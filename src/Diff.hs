module Diff where

import Syntax
import Data.Map
import Data.Maybe
import Data.Set
import Control.Monad.Free
import Control.Comonad.Cofree
import Patch
import Term
import Categorizable

data Annotated a annotation f = Annotated annotation (Syntax a f)
  deriving (Functor, Eq, Show)

data Range = Range { start :: Integer, end :: Integer }
  deriving (Eq, Show)

data Info = Info Range (Set String)
  deriving (Eq, Show)

instance Categorizable Info where
  categories (Info _ c) = c

type Diff a annotation = Free (Annotated a (annotation, annotation)) (Patch (Term a annotation))

cost :: Diff a annotation -> Integer
cost f = iter c $ fmap (const 1) f where
  c (Annotated _ (Leaf _)) = 0
  c (Annotated _ (Keyed xs)) = sum $ snd <$> Data.Map.toList xs
  c (Annotated _ (Indexed xs)) = sum xs
  c (Annotated _ (Fixed xs)) = sum xs

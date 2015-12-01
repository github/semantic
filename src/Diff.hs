module Diff where

import Syntax
import Data.Map
import Data.Set
import Control.Monad.Free
import Patch
import Term
import Categorizable

data Annotated a annotation f = Annotated annotation (Syntax a f)
  deriving (Functor, Eq, Show, Foldable)

data Range = Range { start :: Int, end :: Int }
  deriving (Eq, Show)

type Category = String
data Info = Info Range (Set Category)
  deriving (Eq, Show)

instance Categorizable Info where
  categories (Info _ c) = c

type Diff a annotation = Free (Annotated a (annotation, annotation)) (Patch (Term a annotation))

diffSum :: (Patch (Term a annotation) -> Integer) -> Diff a annotation -> Integer
diffSum patchCost diff = iter (c . unwrap) $ fmap patchCost diff where
  c (Leaf _) = 0
  c (Keyed xs) = sum $ snd <$> Data.Map.toList xs
  c (Indexed xs) = sum xs
  c (Fixed xs) = sum xs
  unwrap (Annotated _ syntax) = syntax

diffCost :: Diff a annotation -> Integer
diffCost = diffSum $ patchSum termSize

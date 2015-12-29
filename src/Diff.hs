module Diff where

import Syntax
import Data.Set
import Control.Monad.Free
import Patch
import Term
import Range
import Categorizable

data Annotated a annotation f = Annotated !annotation !(Syntax a f)
  deriving (Functor, Eq, Show, Foldable)

type Category = String
data Info = Info { characterRange :: !Range, categories :: !(Set Category) }
  deriving (Eq, Show)

instance Categorizable Info where
  categories = Diff.categories

instance Monoid Info where
  mempty = Info mempty mempty
  mappend (Info r1 c1) (Info r2 c2) = Info (mappend r1 r2) (mappend c1 c2)

type Diff a annotation = Free (Annotated a (annotation, annotation)) (Patch (Term a annotation))

diffSum :: (Patch (Term a annotation) -> Integer) -> Diff a annotation -> Integer
diffSum patchCost diff = sum $ fmap patchCost diff

diffCost :: Diff a annotation -> Integer
diffCost = diffSum $ patchSum termSize

{-# LANGUAGE LambdaCase #-}
module Data.Edit
( Edit(..)
, edit
, mergeEdit
, mergeEditWith
) where

import Control.Applicative (liftA2)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)

-- | The deletion, insertion, or comparison of values.
data Edit a b
  = Delete a
  | Insert b
  | Compare a b
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)


-- | Return both sides of an edit.
edit :: (l -> a) -> (r -> a) -> (l -> r -> a) -> Edit l r -> a
edit delete insert compare = \case
  Delete  a   -> delete a
  Insert    b -> insert b
  Compare a b -> compare a b

mergeEdit :: (a -> a -> a) -> Edit a a -> a
mergeEdit = edit id id

mergeEditWith :: (l -> a) -> (r -> a) -> (a -> a -> a) -> Edit l r -> a
mergeEditWith f g h = mergeEdit h . bimap f g


instance Bifunctor Edit where
  bimap = bimapDefault

instance Bifoldable Edit where
  bifoldMap = bifoldMapDefault

instance Bitraversable Edit where
  bitraverse f g = edit (fmap Delete) (fmap Insert) (liftA2 Compare) . bimap f g

instance Eq2 Edit where
  liftEq2 eql eqr = curry $ \case
    (Delete  a1   , Delete  a2   ) -> eql a1 a2
    (Insert     b1, Insert     b2) ->              eqr b1 b2
    (Compare a1 b1, Compare a2 b2) -> eql a1 a2 && eqr b1 b2
    _                              -> False

instance Ord2 Edit where
  liftCompare2 cmpl cmpr = curry $ \case
    (Delete  a1   , Delete  a2   ) -> cmpl a1 a2
    (Delete  _    , _            ) -> LT
    (Insert     b1, Insert     b2) ->               cmpr b1 b2
    (Insert     _ , _            ) -> LT
    (Compare a1 b1, Compare a2 b2) -> cmpl a1 a2 <> cmpr b1 b2
    _                              -> GT

instance Show2 Edit where
  liftShowsPrec2 spl _ spr _ d = \case
    Delete  a   -> showsUnaryWith  spl     "Delete"  d a
    Insert    b -> showsUnaryWith      spr "Insert"  d   b
    Compare a b -> showsBinaryWith spl spr "Compare" d a b

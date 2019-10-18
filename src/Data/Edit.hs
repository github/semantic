{-# LANGUAGE LambdaCase #-}
module Data.Edit
( Edit(..)
, edit
, mergeEdit
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)

-- | An operation to compare, insert, or delete an item.
data Edit a b
  = Delete a
  | Insert b
  | Compare a b
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)


-- | Return both sides of a patch.
edit :: (l -> a) -> (r -> a) -> (l -> r -> a) -> Edit l r -> a
edit delete insert compare = \case
  Delete  a   -> delete a
  Insert    b -> insert b
  Compare a b -> compare a b

mergeEdit :: (a -> a -> a) -> Edit a a -> a
mergeEdit = edit id id


instance Bifunctor Edit where
  bimap = bimapDefault

instance Bifoldable Edit where
  bifoldMap = bifoldMapDefault

instance Bitraversable Edit where
  bitraverse f g = \case
    Delete  a   -> Delete  <$> f a
    Insert    b -> Insert  <$>         g b
    Compare a b -> Compare <$> f a <*> g b

instance Eq2 Edit where
  liftEq2 eqBefore eqAfter p1 p2 = case (p1, p2) of
    (Delete a1, Delete a2) -> eqBefore a1 a2
    (Insert b1, Insert b2) -> eqAfter b1 b2
    (Compare a1 b1, Compare a2 b2) -> eqBefore a1 a2 && eqAfter b1 b2
    _ -> False

instance Show2 Edit where
  liftShowsPrec2 spl _ spr _ d = \case
    Delete a -> showsUnaryWith spl "Delete" d a
    Insert b -> showsUnaryWith spr "Insert" d b
    Compare a b -> showsBinaryWith spl spr "Compare" d a b

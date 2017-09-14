{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Patch
( Patch(..)
, after
, before
, patch
) where

import Data.Aeson
import Data.Align
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Symmetrical
import Data.Bitraversable
import Data.Functor.Classes
import Data.JSON.Fields
import Data.These
import GHC.Generics

-- | An operation to replace, insert, or delete an item.
data Patch a b
  = Delete a
  | Insert b
  | Replace a b
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)


-- | Return the item from the after side of the patch.
after :: Patch before after -> Maybe after
after = patch (const Nothing) Just (\ _ b -> Just b)

-- | Return the item from the before side of the patch.
before :: Patch before after -> Maybe before
before = patch Just (const Nothing) (\ a _ -> Just a)

-- | Return both sides of a patch.
patch :: (before -> result) -> (after -> result) -> (before -> after -> result) -> Patch before after -> result
patch ifDelete _ _ (Delete a) = ifDelete a
patch _ ifInsert _ (Insert b) = ifInsert b
patch _ _ ifReplace (Replace a b) = ifReplace a b


-- Instances

instance Bifunctor Patch where
  bimap f _ (Delete a) = Delete (f a)
  bimap _ g (Insert b) = Insert (g b)
  bimap f g (Replace a b) = Replace (f a) (g b)

instance Symmetrical Patch where
  mirror = patch Insert Delete (flip Replace)

instance Bifoldable Patch where
  bifoldMap f _ (Delete a) = f a
  bifoldMap _ g (Insert b) = g b
  bifoldMap f g (Replace a b) = f a `mappend` g b

instance Bitraversable Patch where
  bitraverse f _ (Delete a) = Delete <$> f a
  bitraverse _ g (Insert b) = Insert <$> g b
  bitraverse f g (Replace a b) = Replace <$> f a <*> g b

instance Bicrosswalk Patch where
  bicrosswalk f _ (Delete a) = Delete <$> f a
  bicrosswalk _ g (Insert b) = Insert <$> g b
  bicrosswalk f g (Replace a b) = alignWith (these Delete Insert Replace) (f a) (g b)

instance Eq2 Patch where
  liftEq2 eqBefore eqAfter p1 p2 = case (p1, p2) of
    (Delete a1, Delete a2) -> eqBefore a1 a2
    (Insert b1, Insert b2) -> eqAfter b1 b2
    (Replace a1 b1, Replace a2 b2) -> eqBefore a1 a2 && eqAfter b1 b2
    _ -> False

instance Show2 Patch where
  liftShowsPrec2 spBefore _ spAfter _ d p = case p of
    Delete a -> showsUnaryWith spBefore "Delete" d a
    Insert b -> showsUnaryWith spAfter "Insert" d b
    Replace a b -> showsBinaryWith spBefore spAfter "Replace" d a b


instance (ToJSONFields a, ToJSONFields b) => ToJSONFields (Patch a b) where
  toJSONFields (Insert a)    = [ "insert" .= object (toJSONFields a) ]
  toJSONFields (Delete a)    = [ "delete" .= object (toJSONFields a) ]
  toJSONFields (Replace a b) = [ "replace" .= [object (toJSONFields a), object (toJSONFields b)] ]

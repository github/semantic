{-# LANGUAGE TypeOperators #-}
module Data.Mergeable.Generic where

import Control.Applicative
import GHC.Generics

-- Classes

class GMergeable t where
  gmerge :: Alternative f => (a -> f b) -> t a -> f (t b)

genericMerge :: (Generic1 t, GMergeable (Rep1 t), Alternative f) => (a -> f b) -> t a -> f (t b)
genericMerge f = fmap to1 . gmerge f . from1


-- Instances

instance GMergeable U1 where
  gmerge _ _ = pure U1

instance GMergeable Par1 where
  gmerge f (Par1 a) = Par1 <$> f a

instance GMergeable (K1 i c) where
  gmerge _ (K1 a) = pure (K1 a)

instance GMergeable f => GMergeable (Rec1 f) where
  gmerge f (Rec1 a) = Rec1 <$> gmerge f a

instance GMergeable f => GMergeable (M1 i c f) where
  gmerge f (M1 a) = M1 <$> gmerge f a

instance (GMergeable f, GMergeable g) => GMergeable (f :+: g) where
  gmerge f (L1 a) = L1 <$> gmerge f a
  gmerge f (R1 b) = R1 <$> gmerge f b

instance (GMergeable f, GMergeable g) => GMergeable (f :*: g) where
  gmerge f (a :*: b) = (:*:) <$> gmerge f a <*> gmerge f b

instance GMergeable [] where
  gmerge f (x:xs) = ((:) <$> f x <|> pure id) <*> gmerge f xs
  gmerge _ [] = pure []

instance GMergeable Maybe where
  gmerge f (Just a) = Just <$> f a
  gmerge _ Nothing = pure empty

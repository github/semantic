{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Data.Functor.Union where

-- | N-ary union of type constructors.
data Union (ts :: [* -> *]) (a :: *) where
  Here :: f a -> Union (f ': ts) a
  There :: Union ts a -> Union (f ': ts) a

{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Serializing.SExpression.Precise
( serializeSExpression
, ToSExpression(..)
) where

import Data.ByteString.Builder
import Data.Foldable (fold)
import Data.List (intersperse)
import GHC.Generics

serializeSExpression :: ToSExpression t => t ann -> Builder
serializeSExpression t = toSExpression t 0 <> "\n"


nl :: Int -> Builder
nl n | n <= 0    = ""
     | otherwise = "\n"

pad :: Int -> Builder
pad n = stringUtf8 (replicate (2 * n) ' ')


class ToSExpression t where
  toSExpression :: t ann -> Int -> Builder

instance (ToSExpressionBy strategy t, strategy ~ ToSExpressionStrategy t) => ToSExpression t where
  toSExpression = toSExpression' @strategy


data Strategy = Generic

type family ToSExpressionStrategy (t :: * -> *) :: Strategy where
  ToSExpressionStrategy _    = 'Generic

class ToSExpressionBy (strategy :: Strategy) t where
  toSExpression' :: t ann -> Int -> Builder

instance (Generic1 t, GToSExpression (Rep1 t)) => ToSExpressionBy 'Generic t where
  toSExpression' t n = nl n <> pad n <> "(" <> fold (intersperse " " (gtoSExpression (from1 t) n)) <> ")"


class GToSExpression f where
  gtoSExpression :: f ann -> Int -> [Builder]

instance GToSExpression f => GToSExpression (M1 D d f) where
  gtoSExpression = gtoSExpression . unM1

instance (GToSExpression f, GToSExpression g) => GToSExpression (f :+: g) where
  gtoSExpression (L1 l) = gtoSExpression l
  gtoSExpression (R1 r) = gtoSExpression r

instance (Constructor c, GToSExpression f) => GToSExpression (M1 C c f) where
  gtoSExpression m n = stringUtf8 (conName m) : gtoSExpression (unM1 m) (n + 1)

instance (GToSExpression f, GToSExpression g) => GToSExpression (f :*: g) where
  gtoSExpression (l :*: r) = gtoSExpression l <> gtoSExpression r

instance GToSExpression U1 where
  gtoSExpression _ _ = []

instance GToSExpression f => GToSExpression (M1 S s f) where
  gtoSExpression = gtoSExpression . unM1 -- FIXME: show the selector name, if any

instance Show k => GToSExpression (K1 R k) where
  gtoSExpression k _ = pure (stringUtf8 (show (unK1 k)))

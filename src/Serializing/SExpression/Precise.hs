{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Serializing.SExpression.Precise
( serializeSExpression
, ToSExpression(..)
) where

import Data.ByteString.Builder
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Text (Text)
import GHC.Generics

serializeSExpression :: ToSExpression t => t -> Builder
serializeSExpression t = toSExpression t 0 <> "\n"


nl :: Int -> Builder
nl n | n <= 0    = ""
     | otherwise = "\n"

pad :: Int -> Builder
pad n = stringUtf8 (replicate (2 * n) ' ')


class ToSExpression t where
  toSExpression :: t -> Int -> Builder

instance (ToSExpressionBy strategy t, strategy ~ ToSExpressionStrategy t) => ToSExpression t where
  toSExpression = toSExpressionWithStrategy @strategy undefined


data Strategy = Generic | Show

type family ToSExpressionStrategy t :: Strategy where
  ToSExpressionStrategy Text = 'Show
  ToSExpressionStrategy _    = 'Generic

class ToSExpressionBy (strategy :: Strategy) t where
  toSExpressionWithStrategy :: proxy strategy -> t -> Int -> Builder

instance Show t => ToSExpressionBy 'Show t where
  toSExpressionWithStrategy _ t _ = stringUtf8 (show t)

instance (Generic t, GToSExpression (Rep t)) => ToSExpressionBy 'Generic t where
  toSExpressionWithStrategy _ t n = nl n <> pad n <> "(" <> fold (intersperse " " (gtoSExpression (from t) n)) <> ")"


class GToSExpression f where
  gtoSExpression :: f (Int -> Builder) -> (Int -> [Builder])

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

instance ToSExpression k => GToSExpression (K1 R k) where
  gtoSExpression k = pure . toSExpression (unK1 k)

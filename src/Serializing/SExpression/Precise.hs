{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Serializing.SExpression.Precise
( serializeSExpression
) where

import Data.ByteString.Builder
import Data.Foldable (fold)
import Data.List (intersperse)
import GHC.Generics

serializeSExpression :: (Generic t, GToSExpression (Rep t)) => t -> Builder
serializeSExpression t = gtoSExpression (from t) 0 <> "\n"

gtoSExpression :: GToSExpression f => f (Int -> Builder) -> (Int -> Builder)
gtoSExpression f n = "(" <> fold (intersperse " " (gtoSExpression' f n)) <> ")"

class GToSExpression f where
  gtoSExpression' :: f (Int -> Builder) -> (Int -> [Builder])

instance GToSExpression f => GToSExpression (M1 D d f) where
  gtoSExpression' = gtoSExpression' . unM1

instance (Constructor c, GToSExpression f) => GToSExpression (M1 C c f) where
  gtoSExpression' m n = stringUtf8 (conName m) : gtoSExpression' (unM1 m) (n + 1)

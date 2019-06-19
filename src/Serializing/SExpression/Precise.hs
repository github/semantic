{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Serializing.SExpression.Precise
( serializeSExpression
) where

import Data.ByteString.Builder
import GHC.Generics

serializeSExpression :: (Generic t, GToSExpression (Rep t)) => t -> Builder
serializeSExpression t = gtoSExpression (from t) 0 <> "\n"

class GToSExpression f where
  gtoSExpression :: f (Int -> Builder) -> (Int -> Builder)

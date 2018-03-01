{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}
module Data.Syntax.Comment where

import Prologue
import Data.Abstract.Evaluatable
import Diffing.Algorithm

-- | An unnested comment (line or block).
newtype Comment a = Comment { commentContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Comment where liftEq = genericLiftEq
instance Ord1 Comment where liftCompare = genericLiftCompare
instance Show1 Comment where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Comment where
  eval _ = pure unit

-- TODO: nested comment types
-- TODO: documentation comment types
-- TODO: literate programming comment types? alternatively, consider those as markup
-- TODO: Differentiate between line/block comments?

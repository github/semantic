{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}
module Data.Syntax.Comment where

import Data.Abstract.Evaluatable
import Data.Abstract.FreeVariables
import Data.Abstract.Value as Value
import Data.Align.Generic
import Data.ByteString (ByteString)
import Data.Functor.Classes.Generic
import Data.Mergeable
import Diffing.Algorithm
import GHC.Generics

-- | An unnested comment (line or block).
newtype Comment a = Comment { commentContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Comment where liftEq = genericLiftEq
instance Ord1 Comment where liftCompare = genericLiftCompare
instance Show1 Comment where liftShowsPrec = genericLiftShowsPrec

instance (AbstractValue v) => Evaluatable es t v Comment where
  eval _ = pure unit

-- TODO: nested comment types
-- TODO: documentation comment types
-- TODO: literate programming comment types? alternatively, consider those as markup
-- TODO: Differentiate between line/block comments?

{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}
module Data.Syntax.Comment where

import Abstract.Eval
import Abstract.Value
import Abstract.Primitive
import Abstract.Type
import Abstract.FreeVariables
import Algorithm
import Data.Align.Generic
import Data.ByteString (ByteString)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Mergeable
import GHC.Generics

-- | An unnested comment (line or block).
newtype Comment a = Comment { commentContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Comment where liftEq = genericLiftEq
instance Ord1 Comment where liftCompare = genericLiftCompare
instance Show1 Comment where liftShowsPrec = genericLiftShowsPrec

instance (Monad m) => Eval l (Value s a l) m t Comment where
  eval _ yield _ = yield (I PUnit)

instance (Monad m) => Eval l Type m t Comment where
  eval _ yield _ = yield Unit

-- TODO: nested comment types
-- TODO: documentation comment types
-- TODO: literate programming comment types? alternatively, consider those as markup
-- TODO: Differentiate between line/block comments?

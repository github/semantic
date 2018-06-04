{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}
module Data.Syntax.Comment where

import Prologue
import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm

-- | An unnested comment (line or block).
newtype Comment a = Comment { commentContent :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Comment where liftEq = genericLiftEq
instance Ord1 Comment where liftCompare = genericLiftCompare
instance Show1 Comment where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Comment where
  toJSONFields1 f@Comment{..} = withChildren f ["contents" .= commentContent ]

instance Evaluatable Comment where
  eval _ = pure (Rval unit)

-- TODO: nested comment types
-- TODO: documentation comment types
-- TODO: literate programming comment types? alternatively, consider those as markup
-- TODO: Differentiate between line/block comments?

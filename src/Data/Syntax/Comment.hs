{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Syntax.Comment (module Data.Syntax.Comment) where

import Data.Abstract.Evaluatable
import Data.Functor.Classes.Generic
import Data.Hashable.Lifted
import Data.JSON.Fields
import Data.Text (Text)
import Diffing.Algorithm
import GHC.Generics (Generic1)

-- | An unnested comment (line or block).
newtype Comment a = Comment { commentContent :: Text }
  deriving (Diffable, Foldable, Functor, Generic1, Hashable1, Traversable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 Comment where liftEq = genericLiftEq
instance Ord1 Comment where liftCompare = genericLiftCompare
instance Show1 Comment where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Comment where
  eval _ _ _ = unit

-- TODO: nested comment types
-- TODO: documentation comment types
-- TODO: literate programming comment types? alternatively, consider those as markup
-- TODO: Differentiate between line/block comments?

-- | HashBang line (e.g. `#!/usr/bin/env node`)
newtype HashBang a = HashBang { value :: Text }
  deriving (Diffable, Foldable, Functor, Generic1, Hashable1, Traversable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 HashBang where liftEq = genericLiftEq
instance Ord1 HashBang where liftCompare = genericLiftCompare
instance Show1 HashBang where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for HashBang
instance Evaluatable HashBang

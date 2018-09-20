{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Comment where

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Prologue
import Proto3.Suite.Class
import Reprinting.Tokenize as Token

-- | An unnested comment (line or block).
newtype Comment a = Comment { commentContent :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 Comment where liftEq = genericLiftEq
instance Ord1 Comment where liftCompare = genericLiftCompare
instance Show1 Comment where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Comment where
  eval _ _ = rvalBox unit

instance Tokenize Comment where
  tokenize = yield . Run . commentContent

-- TODO: nested comment types
-- TODO: documentation comment types
-- TODO: literate programming comment types? alternatively, consider those as markup
-- TODO: Differentiate between line/block comments?

-- | HashBang line (e.g. `#!/usr/bin/env node`)
newtype HashBang a = HashBang { value :: Text }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Eq1 HashBang where liftEq = genericLiftEq
instance Ord1 HashBang where liftCompare = genericLiftCompare
instance Show1 HashBang where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for HashBang
instance Evaluatable HashBang

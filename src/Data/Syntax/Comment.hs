module Data.Syntax.Comment where

import Data.Functor.Classes.Eq.Generic
import GHC.Generics
import Prologue

-- | An unnested comment (line or block).
newtype Comment a = Comment { commentContent :: ByteString }
  deriving (Eq, Generic1, Show)

instance Eq1 Comment where liftEq = genericLiftEq

-- TODO: nested comment types
-- TODO: documentation comment types
-- TODO: literate programming comment types? alternatively, consider those as markup

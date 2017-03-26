module Data.Syntax.Comment where

import Prologue

-- | An unnested comment (line or block).
newtype Comment a = Comment { commentContent :: ByteString }
  deriving (Eq, Show)

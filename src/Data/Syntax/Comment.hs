module Data.Syntax.Comment where

import Prologue

newtype Comment a = Comment { commentContent :: ByteString }
  deriving (Eq, Show)

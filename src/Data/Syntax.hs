module Data.Syntax where

import Prologue

-- Undifferentiated

newtype Leaf a = Leaf { leafContent :: ByteString }
  deriving (Eq, Show)

newtype Branch a = Branch { branchElements :: [a] }
  deriving (Eq, Show)

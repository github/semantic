module Data.Syntax where

import Prologue

-- Undifferentiated

newtype Leaf a = Leaf { leafContent :: ByteString }
  deriving (Eq, Show)

newtype Branch a = Branch { branchElements :: [a] }
  deriving (Eq, Show)


-- Common

-- | An identifier of some other construct, whether a containing declaration (e.g. a class name) or a reference (e.g. a variable).
newtype Identifier a = Identifier ByteString
  deriving (Eq, Show)

module Data.Syntax where

import Data.Functor.Classes.Eq.Generic
import GHC.Generics
import Prologue

-- Undifferentiated

newtype Leaf a = Leaf { leafContent :: ByteString }
  deriving (Eq, Generic1, Show)

instance Eq1 Leaf where liftEq = genericLiftEq

newtype Branch a = Branch { branchElements :: [a] }
  deriving (Eq, Generic1, Show)

instance Eq1 Branch where liftEq = genericLiftEq


-- Common

-- | An identifier of some other construct, whether a containing declaration (e.g. a class name) or a reference (e.g. a variable).
newtype Identifier a = Identifier ByteString
  deriving (Eq, Generic1, Show)

instance Eq1 Identifier where liftEq = genericLiftEq

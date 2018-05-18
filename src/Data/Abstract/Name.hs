{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Name where

import qualified Data.ByteString.Char8 as BC
import           Data.String
import           Prologue

-- | The type of variable names.
newtype Name = Name ByteString
  deriving (Eq, Hashable, Ord)

name :: ByteString -> Name
name = Name

unName :: Name -> ByteString
unName (Name name) = name

instance IsString Name where
  fromString = Name . BC.pack

instance Show Name where showsPrec d (Name str) = showsPrec d str

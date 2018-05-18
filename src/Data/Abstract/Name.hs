module Data.Abstract.Name
( Name
, name
, nameI
, unName
) where

import qualified Data.ByteString.Char8 as BC
import           Data.String
import           Prologue

-- | The type of variable names.
data Name
  = Name ByteString
  | I Int
  deriving (Eq, Ord)

-- | Construct a 'Name' from a 'ByteString'.
name :: ByteString -> Name
name = Name

-- | Construct a 'Name' from an 'Int'. This is suitable for automatic generation, e.g. using a Fresh effect, but should not be used for human-generated names.
nameI :: Int -> Name
nameI = I

unName :: Name -> ByteString
unName (Name name) = name
unName (I i)       = BC.pack (show i)

instance IsString Name where
  fromString = Name . BC.pack

instance Show Name where
  showsPrec d = showsPrec d . unName

instance Hashable Name where
  hashWithSalt salt (Name name) = hashWithSalt salt name
  hashWithSalt salt (I i)       = salt `hashWithSalt` (1 :: Int) `hashWithSalt` i

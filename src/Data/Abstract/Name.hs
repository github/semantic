module Data.Abstract.Name
( Name
, name
, nameI
, unName
) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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

-- | Extract a human-readable 'ByteString' from a 'Name'.
unName :: Name -> ByteString
unName (Name name) = name
unName (I i)       = Text.encodeUtf8 . Text.pack $ '_' : (alphabet !! a) : replicate n 'ʹ'
  where alphabet = ['a'..'z']
        (n, a) = i `divMod` length alphabet

instance IsString Name where
  fromString = Name . BC.pack

instance Show Name where
  showsPrec _ = prettyShowString . Text.unpack . Text.decodeUtf8 . unName
    where prettyShowString str = showChar '"' . foldr ((.) . prettyChar) id str . showChar '"'
          prettyChar c
            | Char.isPrint c = showChar c
            | otherwise      = Char.showLitChar c

instance Hashable Name where
  hashWithSalt salt (Name name) = hashWithSalt salt name
  hashWithSalt salt (I i)       = salt `hashWithSalt` (1 :: Int) `hashWithSalt` i

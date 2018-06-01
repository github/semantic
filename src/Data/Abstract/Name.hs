{-# LANGUAGE DeriveAnyClass #-}
module Data.Abstract.Name
( Name
-- * Constructors
, name
, nameI
, unName
) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as Char
import           Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Prologue
import Proto3.Suite
import qualified Proto3.Wire.Decode as Decode
import qualified Proto3.Wire.Encode as Encode

-- | The type of variable names.
data Name
  = Name ByteString
  | I Int
  deriving (Eq, Ord, MessageField)

instance HasDefault Name where
  def = Name mempty

instance Primitive Name where
  encodePrimitive num (Name bytes) = Encode.byteString num bytes
  encodePrimitive num (I _) = error "We should never be encoding I :: Name constructors"
  decodePrimitive = Name <$> Decode.byteString
  primType _ = Bytes

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

-- $
-- >>> I 0
-- "_a"
-- >>> I 26
-- "_aʹ"
instance Show Name where
  showsPrec _ = prettyShowString . Text.unpack . Text.decodeUtf8 . unName
    where prettyShowString str = showChar '"' . foldr ((.) . prettyChar) id str . showChar '"'
          prettyChar c
            | c `elem` ['\\', '\"'] = Char.showLitChar c
            | Char.isPrint c        = showChar c
            | otherwise             = Char.showLitChar c

instance Hashable Name where
  hashWithSalt salt (Name name) = hashWithSalt salt name
  hashWithSalt salt (I i)       = salt `hashWithSalt` (1 :: Int) `hashWithSalt` i

instance ToJSON Name where
  toJSON = toJSON . Text.decodeUtf8 . unName
  toEncoding = toEncoding . Text.decodeUtf8 . unName

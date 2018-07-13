{-# LANGUAGE DeriveAnyClass #-}
module Data.Abstract.Name
( Name
-- * Constructors
, gensym
, name
, nameI
, formatName
) where

import           Control.Monad.Effect
import           Control.Monad.Effect.Fresh
import           Data.Aeson
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import           Prologue
import           Proto3.Suite
import qualified Proto3.Wire.Decode as Decode
import qualified Proto3.Wire.Encode as Encode

-- | The type of variable names.
data Name
  = Name Text
  | I Int
  deriving (Eq, Ord, MessageField)

instance HasDefault Name where
  def = Name mempty

instance Primitive Name where
  encodePrimitive num (Name text) = Encode.text num (LT.fromStrict text)
  encodePrimitive num (I index)   = Encode.int num index
  decodePrimitive = Name . LT.toStrict <$> Decode.text <|> I <$> Decode.int
  primType _ = Bytes

-- | Generate a fresh (unused) name for use in synthesized variables/closures/etc.
gensym :: (Functor (m effs), Member Fresh effs, Effectful m) => m effs Name
gensym = I <$> fresh

-- | Construct a 'Name' from a 'Text'.
name :: Text -> Name
name = Name

-- | Construct a 'Name' from an 'Int'. This is suitable for automatic generation, e.g. using a Fresh effect, but should not be used for human-generated names.
nameI :: Int -> Name
nameI = I

-- | Extract a human-readable 'Text' from a 'Name'.
formatName :: Name -> Text
formatName (Name name) = name
formatName (I i)       = Text.pack $ '_' : (alphabet !! a) : replicate n 'ʹ'
  where alphabet = ['a'..'z']
        (n, a) = i `divMod` length alphabet

-- $
-- >>> I 0
-- "_a"
-- >>> I 26
-- "_aʹ"
instance Show Name where
  showsPrec _ = prettyShowString . Text.unpack . formatName
    where prettyShowString str = showChar '"' . foldr ((.) . prettyChar) id str . showChar '"'
          prettyChar c
            | c `elem` ['\\', '\"'] = Char.showLitChar c
            | Char.isPrint c        = showChar c
            | otherwise             = Char.showLitChar c

instance Hashable Name where
  hashWithSalt salt (Name name) = hashWithSalt salt name
  hashWithSalt salt (I i)       = salt `hashWithSalt` (1 :: Int) `hashWithSalt` i

instance ToJSON Name where
  toJSON = toJSON . formatName
  toEncoding = toEncoding . formatName

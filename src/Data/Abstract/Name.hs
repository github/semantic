{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Abstract.Name
( Name
-- * Constructors
, gensym
, name
, nameI
, formatName
, __self
) where

import           Control.Effect.Fresh
import           Data.Aeson
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prologue

-- | The type of variable names.
data Name
  = Name Text
  | I Int
  deriving (Eq, Ord)

-- | Generate a fresh (unused) name for use in synthesized variables/closures/etc.
gensym :: (Member Fresh sig, Carrier sig m) => m Name
gensym = I <$> fresh

-- | Construct a 'Name' from a 'Text'.
name :: Text -> Name
name = Name

-- | Construct a 'Name' from an 'Int'. This is suitable for automatic generation, e.g. using a Fresh effect, but should not be used for human-generated names.
nameI :: Int -> Name
nameI = I

-- | Extract a human-readable 'Text' from a 'Name'.
-- Sample outputs can be found in @Data.Abstract.Name.Spec@.
formatName :: Name -> Text
formatName (Name name) = name
formatName (I i)       = Text.pack $ '_' : (alphabet !! a) : replicate n 'ʹ'
  where alphabet = ['a'..'z']
        (n, a) = i `divMod` length alphabet

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

__self :: Name
__self = name "__semantic_self"

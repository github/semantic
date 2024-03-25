{-# LANGUAGE FlexibleContexts #-}
module Analysis.Name
( Name
  -- * Constructors
, gensym
, name
, nameI
, formatName
, isGenerated
  -- * De Bruijn indices & levels
, Level(..)
, Index(..)
  -- ** Conversions
, levelToIndex
, indexToLevel
) where

import           Control.Effect.Fresh
import           Data.Aeson
import qualified Data.Char as Char
import           Data.Hashable
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text

-- | The type of variable names.
data Name
  = Name Text
  | I Int
  deriving (Eq, Ord)

instance IsString Name where
  fromString = Name . fromString

-- | Generate a fresh (unused) name for use in synthesized variables/closures/etc.
gensym :: Has Fresh sig m => m Name
gensym = I <$> fresh

-- | Construct a 'Name' from a 'Text'.
name :: Text -> Name
name = Name

isGenerated :: Name -> Bool
isGenerated (I _) = True
isGenerated _     = False

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


-- De Bruijn indices & levels

-- | De Bruijn levels.
newtype Level = Level { getLevel :: Int }
  deriving (Eq, Ord, Show)

-- | De Bruijn indicex.
newtype Index = Index { getIndex :: Int }
  deriving (Eq, Ord, Show)


-- Conversions

levelToIndex :: Level -> Level -> Index
levelToIndex (Level d) (Level level) = Index (d - level - 1)

indexToLevel :: Level -> Index -> Level
indexToLevel (Level d) (Index index) = Level (d - index - 1)

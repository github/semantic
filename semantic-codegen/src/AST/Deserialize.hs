{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}

-- Turn off partial field warnings for Datatype.
{-# OPTIONS_GHC -Wno-partial-fields #-}
module AST.Deserialize
( Datatype (..)
, Field (..)
, Children(..)
, Required (..)
, Type (..)
, DatatypeName (..)
, Named (..)
, Multiple (..)
) where

import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Char
import GHC.Generics hiding (Constructor, Datatype)
import Language.Haskell.TH.Syntax (Lift)
import Data.Text (Text, unpack)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

-- Types to deserialize into:
data Datatype
  = SumType
  { datatypeName       :: DatatypeName
  , datatypeNameStatus :: Named
  , datatypeSubtypes   :: NonEmpty Type
  }
  | ProductType
  { datatypeName       :: DatatypeName
  , datatypeNameStatus :: Named
  , datatypeChildren   :: Maybe Children
  , datatypeFields     :: [(String, Field)]
  }
  | LeafType
  { datatypeName       :: DatatypeName
  , datatypeNameStatus :: Named
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Datatype where
  parseJSON = withObject "Datatype" $ \v -> do
    type' <- v .: "type"
    named <- v .: "named"
    subtypes <- v .:? "subtypes"
    case subtypes of
      Nothing -> do
        fields <- fmap (fromMaybe HM.empty) (v .:? "fields")
        children <- v .:? "children"
        if null fields && null children then
          pure (LeafType type' named)
        else
          ProductType type' named children <$> parseKVPairs (HM.toList fields)
      Just subtypes   -> pure (SumType type' named subtypes)


-- | Transforms list of key-value pairs to a Parser
parseKVPairs :: [(Text, Value)] -> Parser [(String, Field)]
parseKVPairs = traverse go
  where go :: (Text, Value) -> Parser (String, Field)
        go (t,v) = do
          v' <- parseJSON v
          pure (unpack t, v')

data Field = MkField
  { fieldRequired :: Required
  , fieldTypes    :: NonEmpty Type
  , fieldMultiple :: Multiple
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Field where
  parseJSON = genericParseJSON customOptions


newtype Children = MkChildren Field
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)


data Required = Optional | Required
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Required where
  parseJSON = withBool "Required" (\p -> pure (if p then Required else Optional))

data Type = MkType
  { fieldType :: DatatypeName
  , isNamed :: Named
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Type where
  parseJSON = genericParseJSON customOptions

newtype DatatypeName = DatatypeName { getDatatypeName :: String }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data Named = Anonymous | Named
  deriving (Eq, Ord, Show, Generic, ToJSON, Lift)

instance FromJSON Named where
  parseJSON = withBool "Named" (\p -> pure (if p then Named else Anonymous))

data Multiple = Single | Multiple
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance FromJSON Multiple where
  parseJSON = withBool "Multiple" (\p -> pure (if p then Multiple else Single))

customOptions :: Aeson.Options
customOptions = Aeson.defaultOptions
  {
    fieldLabelModifier = initLower . dropPrefix
  , constructorTagModifier = initLower
  }

dropPrefix :: String -> String
dropPrefix = Prelude.dropWhile isLower

initLower :: String -> String
initLower (c:cs) = toLower c : cs
initLower "" = ""
{-# LANGUAGE DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, LambdaCase, StandaloneDeriving, FlexibleInstances, NamedFieldPuns, OverloadedStrings, QuantifiedConstraints, TypeOperators, UndecidableInstances, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances () where

-- Testing code depends on certain instances that we don't want to
-- expose in semantic-core proper, yet are important enough that
-- we should keep track of them in a dedicated file.

import           Analysis.ScopeGraph
import           Data.Aeson
import           Data.File
import           Data.Loc
import qualified Data.Map as Map
import           Data.Name (Name (..))
import           Data.Text (Text)

deriving newtype instance ToJSON Name
deriving newtype instance ToJSONKey Name

instance ToJSON a => ToJSON (File a) where
  toJSON File{filePath, fileSpan, fileBody} = object
    [ "path" .= filePath
    , "span" .= fileSpan
    , "body" .= fileBody
    ]

deriving newtype instance ToJSON Path

instance ToJSON Ref where
  toJSON (Ref path span) = object
    [ "kind" .= ("ref" :: Text)
    , "path" .= path
    , "span" .= span
    ]

instance ToJSON Decl where
  toJSON Decl{declSymbol, declPath, declSpan} = object
    [ "kind"   .= ("decl" :: Text)
    , "symbol" .= declSymbol
    , "path" .= declPath
    , "span" .= declSpan
    ]

instance ToJSON ScopeGraph where
  toJSON (ScopeGraph sc) = toJSON . Map.mapKeys declSymbol $ sc

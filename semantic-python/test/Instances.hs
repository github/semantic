{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, NamedFieldPuns, OverloadedStrings, QuantifiedConstraints, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances () where

-- Testing code depends on certain instances that we don't want to
-- expose in semantic-core proper, yet are important enough that
-- we should keep track of them in a dedicated file.

import           Analysis.File
import           Analysis.ScopeGraph
import           Core.Name (Name (..))
import           Data.Aeson
import qualified Data.Map as Map
import           Data.Text (Text, pack)
import qualified System.Path as Path

deriving newtype instance ToJSON Name
deriving newtype instance ToJSONKey Name

instance ToJSON a => ToJSON (File a) where
  toJSON File{filePath, fileSpan, fileBody} = object
    [ "path" .= filePath
    , "span" .= fileSpan
    , "body" .= fileBody
    ]

instance ToJSON Path.AbsRelFile where
  toJSON p = toJSON (pack (Path.toString p))

instance ToJSON Ref where
  toJSON (Ref path span) = object
    [ "kind" .= ("ref" :: Text)
    , "path" .= path
    , "span" .= span
    ]

instance ToJSON (Decl Name) where
  toJSON Decl{declSymbol, declPath, declSpan} = object
    [ "kind"   .= ("decl" :: Text)
    , "symbol" .= declSymbol
    , "path" .= declPath
    , "span" .= declSpan
    ]

instance ToJSON (ScopeGraph Name) where
  toJSON (ScopeGraph sc) = toJSON . Map.mapKeys declSymbol $ sc

{-# LANGUAGE DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, LambdaCase, StandaloneDeriving, FlexibleInstances, NamedFieldPuns, OverloadedStrings, QuantifiedConstraints, TypeOperators, UndecidableInstances, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances () where

-- Testing code depends on certain instances that we don't want to
-- expose in semantic-core proper, yet are important enough that
-- we should keep track of them in a dedicated file.

import           Analysis.ScopeGraph
import           Control.Effect.Sum
import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.Loc
import           Data.Core (Core, Ann (..))
import qualified Data.Map as Map
import           Data.File
import           Data.Term
import           Data.Text (Text)
import           Data.Scope (Scope, Incr)
import qualified Data.Scope as Scope
import           Data.Name

instance ToJSON a => ToJSON (File a) where
  toJSON File{fileLoc, fileBody} = object
    [ "location" .= fileLoc
    , "body" .= fileBody
    ]

instance ToJSON Span where
  toJSON Span{spanStart, spanEnd} = object
    [ "kind"  .= ("span" :: Text)
    , "start" .= spanStart
    , "end"   .= spanEnd
    ]

instance ToJSON Pos where
  toJSON Pos{posLine, posCol} = object
    [ "kind" .= ("pos" :: Text)
    , "line" .= posLine
    , "column"  .= posCol
    ]

instance ToJSON Loc where
  toJSON Loc{locPath, locSpan} = object
    [ "kind" .= ("loc" :: Text)
    , "path" .= locPath
    , "span" .= locSpan
    ]

instance ToJSON Ref where
  toJSON (Ref loc) = object [ "kind" .= ("ref" :: Text)
                            , "location" .= loc]

instance ToJSON Decl where
  toJSON Decl{declSymbol, declLoc} = object
    [ "kind"   .= ("decl" :: Text)
    , "symbol" .= declSymbol
    , "location" .= declLoc
    ]

instance ToJSON ScopeGraph where
  toJSON (ScopeGraph sc) = toJSON . Map.mapKeys declSymbol $ sc

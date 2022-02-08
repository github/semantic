{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances () where

import           Analysis.File
import           Data.Aeson
import           Data.Text (pack)
import qualified System.Path as Path

instance ToJSON a => ToJSON (File a) where
  toJSON File{filePath, fileSpan, fileBody} = object
    [ "path" .= filePath
    , "span" .= fileSpan
    , "body" .= fileBody
    ]

instance ToJSON Path.AbsRelFile where
  toJSON p = toJSON (pack (Path.toString p))

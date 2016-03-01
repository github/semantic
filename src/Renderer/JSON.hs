{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON (
  json
) where

import Alignment
import Category
import Data.Aeson hiding (json)
import Data.ByteString.Lazy
import Data.OrderedMap
import qualified Data.Text as T
import Diff
import Line
import Range
import Renderer
import Row
import Source
import SplitDiff
import Syntax
import Term

-- | Render a diff to a string representing its JSON.
json :: ToJSON a => Renderer a ByteString
json diff (a, b) = encode $ splitDiffByLines diff (0, 0) (source a, source b)

instance (ToJSON leaf, ToJSON annotation, ToJSON recur) => ToJSON (Annotated leaf annotation recur)
instance ToJSON Category where
  toJSON (Other s) = String $ T.pack s
  toJSON s = String . T.pack $ show s
instance ToJSON Info
instance ToJSON a => ToJSON (Line a)
instance (ToJSON key, ToJSON value) => ToJSON (OrderedMap key value)
instance ToJSON Range
instance ToJSON a => ToJSON (Row a)
instance ToJSON leaf => ToJSON (SplitDiff leaf Info)
instance ToJSON a => ToJSON (SplitPatch a)
instance (ToJSON leaf, ToJSON recur) => ToJSON (Syntax leaf recur)
instance ToJSON leaf => ToJSON (Term leaf Info)

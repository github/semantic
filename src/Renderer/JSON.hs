{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON (
  json
) where

import Alignment
import Category
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Aeson hiding (json)
import Data.ByteString.Lazy
import Data.OrderedMap hiding (fromList)
import qualified Data.Text as T
import Data.Vector hiding (toList)
import Diff
import Line
import Range
import Renderer
import Row
import Source hiding (fromList, toList)
import SplitDiff
import Syntax
import Term

-- | Render a diff to a string representing its JSON.
json :: ToJSON a => Renderer a ByteString
json diff (a, b) = encode . fst $ splitDiffByLines diff (0, 0) (source a, source b)

instance (ToJSON leaf, ToJSON annotation, ToJSON recur) => ToJSON (Annotated leaf annotation recur)
instance ToJSON Category where
  toJSON (Other s) = String $ T.pack s
  toJSON s = String . T.pack $ show s
instance ToJSON a => ToJSON (Line a)
instance ToJSON Range where
  toJSON (Range start end) = Array (fromList [ toJSON start, toJSON end ])
instance ToJSON a => ToJSON (Row a) where
  toJSON (Row left right) = object [ "left" .= toJSON (fromList $ unLine left), "right" .= toJSON (fromList $ unLine right) ]
instance ToJSON leaf => ToJSON (SplitDiff leaf Info) where
  toJSON (Free (Annotated (Info range categories) syntax)) = object [ "range" .= toJSON range, "categories" .= toJSON categories, "syntax" .= toJSON syntax ]
  toJSON (Pure patch) = toJSON patch
instance ToJSON a => ToJSON (SplitPatch a) where
  toJSON (SplitInsert a) = object [ "insert" .= toJSON a ]
  toJSON (SplitDelete a) = object [ "delete" .= toJSON a ]
  toJSON (SplitReplace a) = object [ "replace" .= toJSON a ]
instance (ToJSON leaf, ToJSON recur) => ToJSON (Syntax leaf recur) where
  toJSON (Leaf _) = object [ "type" .= String "leaf" ]
  toJSON (Indexed c) = object [ "type" .= String "indexed", "children" .= Array (fromList $ toJSON <$> c) ]
  toJSON (Fixed c) = object [ "type" .= String "fixed", "children" .= Array (fromList $ toJSON <$> c) ]
  toJSON (Keyed c) = object [ "type" .= String "fixed", "children" .= object (uncurry (.=) <$> toList c) ]
instance ToJSON leaf => ToJSON (Term leaf Info) where
  toJSON (Info range categories :< syntax) = object [ "range" .= toJSON range, "categories" .= toJSON categories, "syntax" .= toJSON syntax ]

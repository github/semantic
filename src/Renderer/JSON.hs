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
import Data.ByteString.Builder
import Data.ByteString.Lazy
import Data.Functor.Both
import Data.Monoid
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
json :: Renderer a ByteString
json diff sources = toLazyByteString . fromEncoding $ pairs ("rows" .= Prelude.fst (splitDiffByLines diff (pure 0) (source <$> sources)))

instance ToJSON Category where
  toJSON (Other s) = String $ T.pack s
  toJSON s = String . T.pack $ show s
instance ToJSON Range where
  toJSON (Range start end) = Array . fromList $ toJSON <$> [ start, end ]
instance ToJSON a => ToJSON (Row a) where
  toJSON (Row (Both (left, right))) = Array . fromList $ toJSON . fromList . unLine <$> [ left, right ]
instance ToJSON (SplitDiff leaf Info) where
  toJSON (Free (Annotated info syntax)) = object (termFields info syntax)
  toJSON (Pure patch) = toJSON patch
  toEncoding (Free (Annotated info syntax)) = pairs (termSeries info syntax)
  toEncoding (Pure patch) = pairs . fields $ case patch of
    SplitInsert a -> ("insert", a)
    SplitDelete a -> ("delete", a)
    SplitReplace a -> ("replace", a)
    where fields (kind, (info :< syntax)) = termSeries info syntax <> "patch" .= T.pack kind
instance ToJSON a => ToJSON (SplitPatch a) where
  toJSON (SplitInsert a) = object [ "insert" .= toJSON a ]
  toJSON (SplitDelete a) = object [ "delete" .= toJSON a ]
  toJSON (SplitReplace a) = object [ "replace" .= toJSON a ]
instance (ToJSON recur) => ToJSON (Syntax leaf recur) where
  toJSON (Leaf _) = object [ "type" .= String "leaf" ]
  toJSON (Indexed c) = object [ "type" .= String "indexed", "children" .= Array (fromList $ toJSON <$> c) ]
  toJSON (Fixed c) = object [ "type" .= String "fixed", "children" .= Array (fromList $ toJSON <$> c) ]
  toJSON (Keyed c) = object [ "type" .= String "fixed", "children" .= object (uncurry (.=) <$> toList c) ]
instance ToJSON (Term leaf Info) where
  toJSON (info :< syntax) = object (termFields info syntax)
  toEncoding (info :< syntax) = pairs (termSeries info syntax)

termSeries :: ToJSON recur => Info -> Syntax leaf recur -> Series
termSeries info = mconcat . termFields info

termFields :: (ToJSON recur, KeyValue kv) => Info -> Syntax leaf recur -> [kv]
termFields (Info range categories) syntax = [ "range" .= toJSON range, "categories" .= toJSON categories, "syntax" .= toJSON syntax ]

patchFields :: KeyValue kv => SplitPatch (Cofree (Syntax leaf) Info) -> [kv]
patchFields patch = case patch of
  SplitInsert term -> fields "insert" term
  SplitDelete term -> fields "delete" term
  SplitReplace term -> fields "replace" term
  where fields kind (info :< syntax) = "patch" .= T.pack kind : termFields info syntax

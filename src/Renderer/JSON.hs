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
import Source hiding (fromList, toList)
import SplitDiff
import Syntax
import Term

-- | Render a diff to a string representing its JSON.
json :: Renderer a ByteString
json diff sources = toLazyByteString . fromEncoding . pairs $
     "rows" .= annotateRows (splitDiffByLines (source <$> sources) diff)
  <> "oids" .= (oid <$> sources)
  <> "paths" .= (path <$> sources)
  where annotateRows = fmap (fmap NumberedLine) . numberedRows

newtype NumberedLine a = NumberedLine (Int, Line a)

instance ToJSON (NumberedLine (SplitDiff leaf Info, Range)) where
  toJSON (NumberedLine (n, a)) = object (lineFields n a)
  toEncoding (NumberedLine (n, a)) = pairs $ mconcat (lineFields n a)
instance ToJSON Category where
  toJSON (Other s) = String $ T.pack s
  toJSON s = String . T.pack $ show s
instance ToJSON Range where
  toJSON (Range start end) = Array . fromList $ toJSON <$> [ start, end ]
  toEncoding (Range start end) = foldable [ start,  end ]
instance ToJSON a => ToJSON (Both a) where
  toJSON (Both (a, b)) = Array . fromList $ toJSON <$> [ a, b ]
  toEncoding = foldable
instance ToJSON (SplitDiff leaf Info) where
  toJSON (Free (Annotated info syntax)) = object (termFields info syntax)
  toJSON (Pure patch) = object (patchFields patch)
  toEncoding (Free (Annotated info syntax)) = pairs $ mconcat (termFields info syntax)
  toEncoding (Pure patch) = pairs $ mconcat (patchFields patch)
instance ToJSON value => ToJSON (OrderedMap T.Text value) where
  toJSON map = object $ uncurry (.=) <$> toList map
  toEncoding map = pairs . mconcat $ uncurry (.=) <$> toList map
instance ToJSON (Term leaf Info) where
  toJSON (info :< syntax) = object (termFields info syntax)
  toEncoding (info :< syntax) = pairs $ mconcat (termFields info syntax)

lineFields :: KeyValue kv => Int -> Line (SplitDiff leaf Info, Range) -> [kv]
lineFields _ (Line []) = []
lineFields n line = [ "number" .= n, "terms" .= unLine (Prelude.fst <$> line), "range" .= unionRanges (Prelude.snd <$> line), "hasChanges" .= hasChanges (Prelude.fst <$> line) ]

termFields :: (ToJSON recur, KeyValue kv) => Info -> Syntax leaf recur -> [kv]
termFields (Info range categories) syntax = "range" .= range : "categories" .= categories : case syntax of
  Leaf _ -> []
  Indexed c -> childrenFields c
  Fixed c -> childrenFields c
  Keyed c -> childrenFields c
  where childrenFields c = [ "children" .= c ]

patchFields :: KeyValue kv => SplitPatch (Cofree (Syntax leaf) Info) -> [kv]
patchFields patch = case patch of
  SplitInsert term -> fields "insert" term
  SplitDelete term -> fields "delete" term
  SplitReplace term -> fields "replace" term
  where fields kind (info :< syntax) = "patch" .= T.pack kind : termFields info syntax

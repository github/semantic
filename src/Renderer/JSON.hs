{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON (
  json
) where

import Prologue hiding (toList)
import Alignment
import Category
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Data.Aeson hiding (json)
import Data.Aeson.Encode
import Data.Functor.Both
import Data.OrderedMap hiding (fromList)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text as T
import Data.Vector hiding (toList)
import Info
import Line
import Range
import Renderer
import Source hiding (fromList)
import SplitDiff
import Syntax
import Term

-- | Render a diff to a string representing its JSON.
json :: Renderer
json diff sources = toStrict . toLazyText . encodeToTextBuilder $ object ["rows" .= annotateRows (splitDiffByLines (source <$> sources) diff), "oids" .= (oid <$> sources), "paths" .= (path <$> sources)]
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
  toJSON splitDiff = case runFree splitDiff of
    (Free (info :< syntax)) -> object (termFields info syntax)
    (Pure patch)            -> object (patchFields patch)
  toEncoding splitDiff = case runFree splitDiff of
    (Free (info :< syntax)) -> pairs $ mconcat (termFields info syntax)
    (Pure patch)            -> pairs $ mconcat (patchFields patch)
instance ToJSON value => ToJSON (OrderedMap T.Text value) where
  toJSON map = object $ uncurry (.=) <$> toList map
  toEncoding map = pairs . mconcat $ uncurry (.=) <$> toList map
instance ToJSON (Term leaf Info) where
  toJSON term     | (info :< syntax) <- runCofree term = object (termFields info syntax)
  toEncoding term | (info :< syntax) <- runCofree term = pairs $ mconcat (termFields info syntax)

lineFields :: KeyValue kv => Int -> Line (SplitDiff leaf Info, Range) -> [kv]
lineFields n line | isEmpty line = []
                  | otherwise = [ "number" .= n
                                , "terms" .= unLine (Prologue.fst <$> line)
                                , "range" .= unionRanges (Prologue.snd <$> line)
                                , "hasChanges" .= hasChanges (Prologue.fst <$> line)
                                ]

termFields :: (ToJSON recur, KeyValue kv) => Info -> Syntax leaf recur -> [kv]
termFields (Info range categories _) syntax = "range" .= range : "categories" .= categories : case syntax of
  Leaf _ -> []
  Indexed c -> childrenFields c
  Fixed c -> childrenFields c
  Keyed c -> childrenFields c
  where childrenFields c = [ "children" .= c ]

patchFields :: KeyValue kv => SplitPatch (Term leaf Info) -> [kv]
patchFields patch = case patch of
  SplitInsert term -> fields "insert" term
  SplitDelete term -> fields "delete" term
  SplitReplace term -> fields "replace" term
  where fields kind term | (info :< syntax) <- runCofree term = "patch" .= T.pack kind : termFields info syntax

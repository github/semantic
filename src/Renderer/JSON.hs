{-# LANGUAGE FlexibleInstances, NoOverloadedStrings #-}
module Renderer.JSON (
  json
) where

import Alignment
import Category
import Control.Arrow
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Foldable
import qualified Data.OrderedMap as Map
import Diff
import Line
import Range
import Renderer
import Row
import Source hiding ((++), toList)
import SplitDiff
import Syntax
import Term
import Text.JSON

-- | Render a diff to a string representing its JSON.
json :: Renderer a String
json diff (a, b) = encode . showJSON . fmap JSONWrapper . fmap (fmap JSONWrapper) . fst $ splitDiffByLines diff (0, 0) (source a, source b)

newtype JSONWrapper a = JSONWrapper { unWrap :: a }

instance JSON a => JSON (JSONWrapper (Row a)) where
  showJSON (JSONWrapper (Row left right)) = JSObject $ toJSObject [("left", showJSON (JSONWrapper left)), ("right", showJSON (JSONWrapper right))]

instance JSON a => JSON (JSONWrapper (Line a)) where
  showJSON (JSONWrapper EmptyLine) = JSNull
  showJSON (JSONWrapper (Line a)) = showJSONs (toList a)

instance JSON (JSONWrapper Range) where
  showJSON (JSONWrapper (Range start end)) = showJSON [ start, end ]

instance JSON (JSONWrapper Info) where
  showJSON (JSONWrapper (Info range categories)) = JSObject $ toJSObject [("range", showJSON (JSONWrapper range)), ("categories", showJSON (JSONWrapper <$> toList categories))]

instance JSON (JSONWrapper Category) where
  showJSON (JSONWrapper (Other s)) = JSString $ toJSString s
  showJSON (JSONWrapper s) = JSString . toJSString $ show s

instance JSON (JSONWrapper (SplitDiff a Info)) where
  showJSON (JSONWrapper (Free (Annotated info syntax))) = JSObject $ toJSObject [("info", showJSON (JSONWrapper info)), ("syntax", showJSON (JSONWrapper $ JSONWrapper <$> syntax))]
  showJSON (JSONWrapper (Pure patch)) = showJSON (JSONWrapper $ JSONWrapper <$> patch)

instance JSON a => JSON (JSONWrapper (SplitPatch a)) where
  showJSON (JSONWrapper (SplitInsert term)) = JSObject $ toJSObject [ ("insert", showJSON term) ]
  showJSON (JSONWrapper (SplitDelete term)) = JSObject $ toJSObject [ ("delete", showJSON term) ]
  showJSON (JSONWrapper (SplitReplace term)) = JSObject $ toJSObject [ ("replace", showJSON term) ]

instance JSON (JSONWrapper (Term leaf Info)) where
  showJSON (JSONWrapper (info :< syntax)) = JSObject $ toJSObject [("info", showJSON (JSONWrapper info)), ("syntax", showJSON (JSONWrapper $ JSONWrapper <$> syntax))]

instance JSON recur => JSON (JSONWrapper (Syntax leaf recur)) where
  showJSON (JSONWrapper (Leaf _)) = JSObject $ toJSObject [ ("type", JSString $ toJSString "leaf") ]
  showJSON (JSONWrapper (Indexed i)) = JSObject $ toJSObject [ ("type", JSString $ toJSString "indexed"), ("children", showJSON i) ]
  showJSON (JSONWrapper (Fixed i)) = JSObject $ toJSObject [ ("type", JSString $ toJSString "fixed"), ("children", showJSON i) ]
  showJSON (JSONWrapper (Keyed k)) = JSObject $ toJSObject [ ("type", JSString $ toJSString "fixed"), ("children", JSObject $ toJSObject ((\ (k, v) -> (show k, showJSON v)) <$> (Map.toList k))) ]

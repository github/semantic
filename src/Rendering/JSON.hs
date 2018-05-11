{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rendering.JSON
( JSONOutput(..)
, toJSONOutput
, renderJSONDiff
, renderJSONDiffs
, renderJSONTerm
, renderJSONTerm'
, renderJSONTerms
) where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson as A
import Data.JSON.Fields
import Data.Blob
import qualified Data.Map as Map
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map.Monoidal as Monoidal
import Data.Output
import Data.Patch
import Prologue

newtype JSONOutput = JSONOutput { unJSONOutput :: Monoidal.Map Text [Value] }
  deriving (Eq, Monoid, Semigroup, Show, ToJSON)

toJSONOutput :: Text -> [Value] -> JSONOutput
toJSONOutput key = JSONOutput . Monoidal.singleton key


instance Output JSONOutput where
  toOutput = toStrict . encode . unJSONOutput


-- | Render a diff to a value representing its JSON.
renderJSONDiff :: ToJSON a => BlobPair -> a -> [Value]
renderJSONDiff blobs diff = pure $
  toJSON (object [ "diff" .= diff, "stat" .= object (pathKey <> toJSONFields statPatch) ])
  where statPatch = these Delete Insert Replace (runJoin blobs)
        pathKey = [ "path" .= pathKeyForBlobPair blobs ]

renderJSONDiffs :: [Value] -> Map.Map Text Value
renderJSONDiffs = Map.singleton "diffs" . toJSON


-- | Render a term to a value representing its JSON.
renderJSONTerm :: ToJSON a => Blob -> a -> [Value]
renderJSONTerm blob content = pure $ toJSON (object ("programNode" .= content : toJSONFields blob))

renderJSONTerm' :: (ToJSON a) => Blob -> a -> [Value]
renderJSONTerm' blob content = pure $ toJSON (object ("ast" .= content : toJSONFields blob))

renderJSONTerms :: [Value] -> Map.Map Text Value
renderJSONTerms = Map.singleton "trees" . toJSON

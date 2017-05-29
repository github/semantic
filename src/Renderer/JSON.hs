{-# LANGUAGE DataKinds, GADTs, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON
( json
, ToJSONFields(..)
) where

import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Aeson as A hiding (json)
import Data.Bifunctor.Join
import Data.Functor.Union
import Data.Record
import Info
import Patch
import Prologue hiding ((++))
import Source
import Syntax as S

--
-- Diffs
--

-- | Render a diff to a string representing its JSON.
json :: (ToJSON a, Foldable t) => t SourceBlob -> a -> [Value]
json blobs root = pure $ object
  [ "root" .= toJSON root
  , "oids" .= toJSON (oid <$> toList blobs)
  , "paths" .= toJSON (path <$> toList blobs)
  ]

instance ToJSON a => ToJSONFields (Join (,) a) where
  toJSONFields (Join (a, b)) = [ "before" .= a, "after" .= b ]

instance ToJSON a => ToJSON (Join (,) a) where
  toJSON = toJSON . toList
  toEncoding = foldable

instance (ToJSONFields a, ToJSONFields (f (Free f a))) => ToJSON (Free f a) where
  toJSON splitDiff = case runFree splitDiff of
    (Free f) -> object (toJSONFields f)
    (Pure p) -> object (toJSONFields p)
  toEncoding splitDiff = case runFree splitDiff of
    (Free f) -> pairs $ mconcat (toJSONFields f)
    (Pure p) -> pairs $ mconcat (toJSONFields p)

instance ToJSONFields (CofreeF f a (Cofree f a)) => ToJSON (Cofree f a) where
  toJSON = object . toJSONFields . runCofree
  toEncoding = pairs . mconcat . toJSONFields . runCofree

class ToJSONFields a where
  toJSONFields :: KeyValue kv => a -> [kv]

instance (ToJSONFields h, ToJSONFields (Record t)) => ToJSONFields (Record (h ': t)) where
  toJSONFields (h :. t) = toJSONFields h <> toJSONFields t

instance ToJSONFields (Record '[]) where
  toJSONFields _ = []

instance ToJSONFields (Record fs) => ToJSON (Record fs) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields

instance ToJSONFields Range where
  toJSONFields Range{..} = ["sourceRange" .= [ start, end ]]

instance ToJSONFields Category where
  toJSONFields c = ["category" .= case c of { Other s -> s ; _ -> toS c }]

instance ToJSONFields SourceSpan where
  toJSONFields sourceSpan = [ "sourceSpan" .= sourceSpan ]

instance ToJSONFields SourceText where
  toJSONFields (SourceText t) = [ "sourceText" .= t ]

instance ToJSONFields a => ToJSONFields (Maybe a) where
  toJSONFields = maybe [] toJSONFields

instance (ToJSONFields a, ToJSONFields (f (Cofree f a))) => ToJSONFields (Cofree f a) where
  toJSONFields = toJSONFields . runCofree

instance (ToJSONFields a, ToJSONFields (f b)) => ToJSONFields (CofreeF f a b) where
  toJSONFields (a :< f) = toJSONFields a <> toJSONFields f

instance (ToJSONFields a, ToJSONFields (f (Free f a))) => ToJSONFields (Free f a) where
  toJSONFields = toJSONFields . runFree

instance (ToJSONFields a, ToJSONFields (f b)) => ToJSONFields (FreeF f a b) where
  toJSONFields (Free f) = toJSONFields f
  toJSONFields (Pure a) = toJSONFields a

instance ToJSON a => ToJSONFields (Patch a) where
  toJSONFields (Insert a) = [ "insert" .= a ]
  toJSONFields (Delete a) = [ "delete" .= a ]
  toJSONFields (Replace a b) = [ "replace" .= [a, b] ]

instance ToJSON a => ToJSONFields [a] where
  toJSONFields list = [ "children" .= list ]

instance ToJSON recur => ToJSONFields (Syntax leaf recur) where
  toJSONFields syntax = [ "children" .= toList syntax ]

instance (Foldable f, ToJSON a, ToJSONFields (Union fs a)) => ToJSONFields (Union (f ': fs) a) where
  toJSONFields (Here f) = [ "children" .= toList f ]
  toJSONFields (There fs) = toJSONFields fs

instance ToJSONFields (Union '[] a) where
  toJSONFields _ = []

instance StringConv [Value] ByteString where
  strConv _ = toS . (<> "\n") . encode

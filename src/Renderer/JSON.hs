{-# LANGUAGE DataKinds, GADTs, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON
( renderJSONDiff
, renderJSONTerm
, ToJSONFields(..)
) where

import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Aeson as A hiding (json)
import Data.Bifunctor.Join
import Data.Blob
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (toList)
import Data.Functor.Both (Both)
import qualified Data.Map as Map
import Data.Output
import Data.Proxy
import Data.Record
import Data.Semigroup ((<>))
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Union
import Diff
import GHC.Generics
import Info
import Language
import Patch
import Syntax as S
import Term

--
-- Diffs
--

-- | Render a diff to a string representing its JSON.
renderJSONDiff :: ToJSON a => Both Blob -> a -> Map.Map Text Value
renderJSONDiff blobs diff = Map.fromList
  [ ("diff", toJSON diff)
  , ("oids", toJSON (decodeUtf8 . blobOid <$> toList blobs))
  , ("paths", toJSON (blobPath <$> toList blobs))
  ]

instance Output (Map.Map Text Value) where
  toOutput = toStrict . (<> "\n") . encode

instance ToJSONFields a => ToJSONFields (Join (,) a) where
  toJSONFields (Join (a, b)) = [ "before" .= object (toJSONFields a), "after" .= object (toJSONFields b) ]

instance ToJSON a => ToJSON (Join (,) a) where
  toJSON = toJSON . toList
  toEncoding = foldable

instance (ToJSONFields a, ToJSONFields (f (Diff f a)), ToJSONFields (f (Term f a))) => ToJSON (Diff f a) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields

instance (ToJSONFields a, ToJSONFields (f (Term f a))) => ToJSON (Term f a) where
  toJSON (a :< f) = object (toJSONFields a <> toJSONFields f)
  toEncoding (a :< f) = pairs (mconcat (toJSONFields a <> toJSONFields f))

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
  toJSONFields c = ["category" .= case c of { Other s -> s ; _ -> pack (show c) }]

instance ToJSONFields Span where
  toJSONFields sourceSpan = [ "sourceSpan" .= sourceSpan ]

instance ToJSONFields a => ToJSONFields (Maybe a) where
  toJSONFields = maybe [] toJSONFields

instance (ToJSONFields a, ToJSONFields (f (Term f a))) => ToJSONFields (Term f a) where
  toJSONFields (a :< f) = toJSONFields a <> toJSONFields f

instance (ToJSONFields a, ToJSONFields (f b)) => ToJSONFields (TermF f a b) where
  toJSONFields (a :<< f) = toJSONFields a <> toJSONFields f

instance (ToJSONFields a, ToJSONFields (f (Diff f a)), ToJSONFields (f (Term f a))) => ToJSONFields (Diff f a) where
  toJSONFields = toJSONFields . unDiff

instance (ToJSONFields a, ToJSONFields (f b), ToJSONFields (f (Term f a))) => ToJSONFields (DiffF f a b) where
  toJSONFields (Copy a f)  = toJSONFields a <> toJSONFields f
  toJSONFields (Patch a) = toJSONFields a

instance ToJSONFields a => ToJSONFields (Patch a) where
  toJSONFields (Insert a)    = [ "insert" .= object (toJSONFields a) ]
  toJSONFields (Delete a)    = [ "delete" .= object (toJSONFields a) ]
  toJSONFields (Replace a b) = [ "replace" .= [object (toJSONFields a), object (toJSONFields b)] ]

instance ToJSON a => ToJSONFields [a] where
  toJSONFields list = [ "children" .= list ]

instance ToJSON recur => ToJSONFields (Syntax recur) where
  toJSONFields syntax = [ "children" .= toList syntax ]

instance (Apply1 Foldable fs, ToJSON a) => ToJSONFields (Union fs a) where
  toJSONFields = apply1 (Proxy :: Proxy Foldable) (\ r -> [ "children" .= toList r ])

instance ToJSONFields (Union '[] a) where
  toJSONFields _ = []

data File a = File { filePath :: FilePath, fileLanguage :: Maybe Language, fileContent :: a }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (File a) where
  toJSON File{..} = object [ "filePath" .= filePath, "language" .= fileLanguage, "programNode" .= fileContent ]

instance Output [Value] where
  toOutput = toStrict . (<> "\n") . encode

renderJSONTerm :: ToJSON a => Blob -> a -> [Value]
renderJSONTerm Blob{..} = pure . toJSON . File blobPath blobLanguage

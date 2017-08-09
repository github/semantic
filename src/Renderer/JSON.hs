{-# LANGUAGE DataKinds, GADTs, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON
( renderJSONDiff
, renderJSONTerm
, ToJSONFields(..)
) where

import Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as CofreeF
import Control.Monad.Free
import qualified Control.Monad.Trans.Free as FreeF
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
import GHC.Generics
import Info
import Language
import Patch
import Syntax as S

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

instance ToJSON a => ToJSONFields (Join (,) a) where
  toJSONFields (Join (a, b)) = [ "before" .= a, "after" .= b ]

instance ToJSON a => ToJSON (Join (,) a) where
  toJSON = toJSON . toList
  toEncoding = foldable

instance (ToJSONFields a, ToJSONFields (f (Free f a))) => ToJSON (Free f a) where
  toJSON = object . toJSONFields
  toEncoding = pairs . mconcat . toJSONFields

instance (ToJSONFields a, ToJSONFields (f (Cofree f a))) => ToJSON (Cofree f a) where
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

instance (ToJSONFields a, ToJSONFields (f (Cofree f a))) => ToJSONFields (Cofree f a) where
  toJSONFields (a :< f) = toJSONFields a <> toJSONFields f

instance (ToJSONFields a, ToJSONFields (f b)) => ToJSONFields (CofreeF.CofreeF f a b) where
  toJSONFields (a CofreeF.:< f) = toJSONFields a <> toJSONFields f

instance (ToJSONFields a, ToJSONFields (f (Free f a))) => ToJSONFields (Free f a) where
  toJSONFields (Free f) = toJSONFields f
  toJSONFields (Pure a) = toJSONFields a

instance (ToJSONFields a, ToJSONFields (f b)) => ToJSONFields (FreeF.FreeF f a b) where
  toJSONFields (FreeF.Free f) = toJSONFields f
  toJSONFields (FreeF.Pure a) = toJSONFields a

instance ToJSON a => ToJSONFields (Patch a) where
  toJSONFields (Insert a) = [ "insert" .= a ]
  toJSONFields (Delete a) = [ "delete" .= a ]
  toJSONFields (Replace a b) = [ "replace" .= [a, b] ]

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

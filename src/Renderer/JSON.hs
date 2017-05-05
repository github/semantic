{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses, DataKinds, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renderer.JSON
( json
, jsonFile
, ToJSONFields(..)
) where

import Alignment
import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import Data.Aeson as A hiding (json)
import Data.Bifunctor.Join
import Data.Functor.Both
import Data.Record
import Data.These
import Data.Vector as Vector hiding (toList)
import Diff
import Info
import Prologue hiding ((++))
import qualified Data.Map as Map
import Source
import SplitDiff
import Syntax as S

--
-- Diffs
--

-- | Render a diff to a string representing its JSON.
json :: (ToJSONFields (Record fields), HasField fields Range) => Both SourceBlob -> Diff (Syntax Text) (Record fields) -> Map Text Value
json blobs diff = Map.fromList [
  ("rows", toJSON (annotateRows (alignDiff (source <$> blobs) diff))),
  ("oids", toJSON (oid <$> blobs)),
  ("paths", toJSON (path <$> blobs)) ]
  where annotateRows :: [Join These a] -> [Join These (NumberedLine a)]
        annotateRows = fmap (fmap NumberedLine) . numberedRows

-- | A numbered 'a'.
newtype NumberedLine a = NumberedLine (Int, a)

instance StringConv (Map Text Value) ByteString where
  strConv _ = toS . (<> "\n") . encode

instance ToJSONFields a => ToJSON (NumberedLine a) where
  toJSON (NumberedLine (n, a)) = object $ "number" .= n : toJSONFields a
  toEncoding (NumberedLine (n, a)) = pairs $ "number" .= n <> mconcat (toJSONFields a)

instance ToJSON a => ToJSON (Join These a) where
  toJSON (Join vs) = A.Array . Vector.fromList $ toJSON <$> these pure pure (\ a b -> [ a, b ]) vs
  toEncoding = foldable

instance ToJSON a => ToJSON (Join (,) a) where
  toJSON (Join (a, b)) = A.Array . Vector.fromList $ toJSON <$> [ a, b ]

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

instance ToJSON a => ToJSONFields (SplitPatch a) where
  toJSONFields (SplitInsert a) = [ "insert" .= a ]
  toJSONFields (SplitDelete a) = [ "delete" .= a ]
  toJSONFields (SplitReplace a) = [ "replace" .= a ]

instance ToJSON recur => ToJSONFields (Syntax leaf recur) where
  toJSONFields syntax = [ "children" .= toList syntax ]


--
-- Parse Trees
--

data File a = File { filePath :: FilePath, fileContent :: a }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (File a) where
  toJSON File{..} = object [ "filePath" .= filePath, "programNode" .= fileContent ]

instance StringConv [Value] ByteString where
  strConv _ = toS . (<> "\n") . encode

jsonFile :: ToJSON a => SourceBlob -> a -> [Value]
jsonFile SourceBlob{..} = pure . toJSON . File path

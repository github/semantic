{-# LANGUAGE DeriveAnyClass #-}
module Data.ImportPath (IsRelative(..), ImportPath(..), importPath, toName, defaultAlias) where

import Prologue
import           Data.Aeson
import qualified Data.Text as T
import Data.Abstract.Name
import           Proto3.Suite
import qualified Proto3.Wire.Decode as Decode
import qualified Proto3.Wire.Encode as Encode
import           Data.Abstract.Path (stripQuotes)
import           System.FilePath.Posix

data IsRelative = Unknown | Relative | NonRelative
  deriving (Bounded, Enum, Finite, MessageField, Named, Eq, Generic, Hashable, Ord, Show, ToJSON, NFData)

instance Primitive IsRelative where
  encodePrimitive = Encode.enum
  decodePrimitive = either (const def) id <$> Decode.enum
  primType _ = Named (Single (nameOf (Proxy @IsRelative)))

instance HasDefault IsRelative where
  def = Unknown

data ImportPath = ImportPath { unPath :: FilePath, pathIsRelative :: IsRelative }
  deriving (Eq, Generic, Hashable, Message, Named, Ord, Show, ToJSON, NFData)

instance MessageField ImportPath where
  encodeMessageField num = Encode.embedded num . encodeMessage (fieldNumber 1)
  decodeMessageField = fromMaybe def <$> Decode.embedded (decodeMessage (fieldNumber 1))
  protoType _ = messageField (Prim $ Named (Single (nameOf (Proxy @ImportPath)))) Nothing

instance HasDefault ImportPath where
  def = ImportPath mempty Unknown

-- TODO: fix the duplication present in this and Python
importPath :: Text -> ImportPath
importPath str = let path = stripQuotes str in ImportPath (T.unpack path) (pathType path)
  where
    pathType xs | not (T.null xs), T.head xs == '.' = Relative -- head call here is safe
                | otherwise = NonRelative

defaultAlias :: ImportPath -> Name
defaultAlias = name . T.pack . takeFileName . unPath

toName :: ImportPath -> Name
toName = name . T.pack . unPath

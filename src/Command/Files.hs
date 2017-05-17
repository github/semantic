{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module Command.Files
( readFile
, transcode
, languageForFilePath
) where

import Prologue hiding (readFile)
import Language
import Source
import qualified Data.ByteString as B
import System.FilePath
import Control.Exception (catch, IOException)
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect


-- | Read a file to a SourceBlob, transcoding to UTF-8 along the way.
readFile :: FilePath -> Maybe Language -> IO SourceBlob
readFile path language = do
  raw <- (Just <$> B.readFile path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe ByteString))
  source <- traverse transcode raw
  pure $ fromMaybe (emptySourceBlob path) (sourceBlob path language <$> source)

-- | Transcode a ByteString to a unicode Source.
transcode :: B.ByteString -> IO Source
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text

-- | Return a language based on a FilePath's extension, or Nothing if extension is not found or not supported.
languageForFilePath :: FilePath -> Maybe Language
languageForFilePath = languageForType . toS . takeExtension

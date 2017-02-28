module SemanticDiff.IO where

import Prelude
import Data.Text
import qualified Data.Text.IO as TextIO
import System.IO
import System.Environment (lookupEnv)

import Control.Exception (catch, IOException)
import qualified Data.ByteString as B1
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect

import Source

writeToOutput :: Maybe FilePath -> Text -> IO ()
writeToOutput output text = case output of
  Nothing -> do
    setEncoding
    TextIO.hPutStrLn stdout text
  Just path -> withFile path WriteMode (`TextIO.hPutStr` text)

  where
    setEncoding = do
      lang <- lookupEnv "LANG"
      case lang of
        -- If LANG is set and isn't the empty string, leave the encoding.
        Just x | x /= "" -> pure ()
        -- Otherwise default to utf8.
        _ -> hSetEncoding stdout utf8

-- | Read the file and convert it to Unicode.
readAndTranscodeFile :: FilePath -> IO Source
readAndTranscodeFile path = do
  size <- fileSize path
  text <- case size of
    0 -> pure B1.empty
    _ -> B1.readFile path
  transcode text

-- From https://github.com/haskell/bytestring/pull/79/files
fileSize :: FilePath -> IO Integer
fileSize f = withBinaryFile f ReadMode $ \h -> do
  -- hFileSize fails if file is not regular file (like /dev/null). Catch
  -- exception and try reading anyway.
  filesz <- catch (hFileSize h) useZeroIfNotRegularFile
  pure $ fromIntegral filesz `max` 0
  where useZeroIfNotRegularFile :: IOException -> IO Integer
        useZeroIfNotRegularFile _ = pure 0

-- | Transcode a file to a unicode source.
transcode :: B1.ByteString -> IO Source
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text

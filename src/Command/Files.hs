module Command.Files
( sourceBlobsFromPaths
, transcode
) where

import Prologue
import Source
import qualified Data.ByteString as B
import System.IO
import Control.Exception (catch, IOException)
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect

-- | For the given absolute file paths, retrieves their source blobs.
sourceBlobsFromPaths :: [FilePath] -> IO [SourceBlob]
sourceBlobsFromPaths filePaths =
  for filePaths (\filePath -> do
                  source <- readAndTranscodeFile filePath
                  pure $ Source.SourceBlob source mempty filePath (Just Source.defaultPlainBlob))

-- | Read the file and convert it to Unicode.
readAndTranscodeFile :: FilePath -> IO Source
readAndTranscodeFile path = do
  size <- fileSize path
  text <- case size of
    0 -> pure B.empty
    _ -> B.readFile path
  transcode text

-- Based on https://github.com/haskell/bytestring/pull/79/files
-- Neccessary to be able to handle /dev/null as a file.
fileSize :: FilePath -> IO Integer
fileSize f = withBinaryFile f ReadMode $ \h -> do
  -- hFileSize fails if file is not regular file (like /dev/null). Catch
  -- exception and return 0 in that case.
  filesz <- catch (hFileSize h) useZeroIfNotRegularFile
  pure $ fromIntegral filesz `max` 0
  where useZeroIfNotRegularFile :: IOException -> IO Integer
        useZeroIfNotRegularFile _ = pure 0

-- | Transcode a file to a unicode source.
transcode :: B.ByteString -> IO Source
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text

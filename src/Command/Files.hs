module Command.Files
( readFile
, transcode
) where

import Prologue hiding (readFile)
import Source
import qualified Data.ByteString as B
import System.IO hiding (readFile)
import Control.Exception (catch, IOException)
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect

-- | Read a file to a SourceBlob. (NOTE: file contents are converted to uncode.)
readFile :: FilePath -> IO SourceBlob
readFile path = do
  -- source <- readFile' path
  -- pure $ sourceBlob source path
  -- TODO: Do we want to swallow IOExceptions for files that don't exist? I'm not sure this is a good idea.
  source <- (Just <$> readFile' path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe Source))
  pure $ fromMaybe (emptySourceBlob path) (flip sourceBlob path <$> source)
  where
    -- | Read a file and safely handle special paths like /dev/null
    readFile' :: FilePath -> IO Source
    readFile' path = do
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

-- | Transcode a ByteString to a unicode Source.
transcode :: B.ByteString -> IO Source
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text

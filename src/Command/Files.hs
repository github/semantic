module Command.Files
( readFile
, transcode
) where

import Prologue hiding (readFile)
import Source
import qualified Data.ByteString as B
import Control.Exception (catch, IOException)
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect

-- | Read a file to a SourceBlob, transcoding to UTF-8 along the way.
readFile :: FilePath -> IO SourceBlob
readFile path = do
  raw <- (Just <$> B.readFile path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe ByteString))
  source <- traverse transcode raw
  pure $ fromMaybe (emptySourceBlob path) (flip sourceBlob path <$> source)

-- | Transcode a ByteString to a unicode Source.
transcode :: B.ByteString -> IO Source
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text

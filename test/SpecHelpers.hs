module SpecHelpers where

import Prologue
import qualified Data.ByteString as B
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect
import Source


-- | Read a file and convert it to Unicode.
readFileToUnicode :: FilePath -> IO Source
readFileToUnicode path = B.readFile path >>= transcode
  where
    -- | Transcode a file to a unicode source.
    transcode :: B.ByteString -> IO Source
    transcode text = fromText <$> do
      match <- Detect.detectCharset text
      converter <- Convert.open match Nothing
      pure $ Convert.toUnicode converter text

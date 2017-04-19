module SpecHelpers where

import Prologue
import qualified Data.ByteString as B
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect
import Source
import Info
import Data.Record
import Syntax
import Language
import Parser
import Parser.Language
import System.FilePath

-- TODO: Write helper functions for parse file and diff files that don't depend on Command.

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

-- | Return a parser based on the FilePath's extension (including the "."). |
-- NB: This is intentionally duplicated from Parser.Language because our tests
-- will always need to be able to select language from file extention whereas
-- the semantic project should eventually depend on exernal language detection.
parserForFilePath :: FilePath -> Parser (Syntax Text) (Record DefaultFields)
parserForFilePath = parserForLanguage . languageForType . toS . takeExtension

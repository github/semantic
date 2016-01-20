module DiffOutput where

import Source
import Term
import Control.Comonad.Cofree
import qualified Data.Text as T
import Diff
import Syntax
import Range
import qualified Data.ByteString.Char8 as B1
import qualified Data.Text.ICU.Detect as Detect
import qualified Data.Text.ICU.Convert as Convert

-- | Replace every string leaf with leaves of the words in the string.
breakDownLeavesByWord :: Source Char -> Term T.Text Info -> Term T.Text Info
breakDownLeavesByWord source = cata replaceIn
  where
    replaceIn info@(Info range categories) (Leaf _) | ranges <- rangesAndWordsInSource range, length ranges > 1 = info :< (Indexed $ makeLeaf categories <$> ranges)
    replaceIn info syntax = info :< syntax
    rangesAndWordsInSource range = rangesAndWordsFrom (start range) (Source.toList $ slice range source)
    makeLeaf categories (range, substring) = Info range categories :< Leaf (T.pack substring)

-- | Transcode a file to a unicode source.
transcode :: B1.ByteString -> IO (Source Char)
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  return $ Convert.toUnicode converter text

readAndTranscodeFile :: FilePath -> IO (Source Char)
readAndTranscodeFile path = do
  text <- B1.readFile path
  transcode text

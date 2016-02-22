module Diffing where

import Diff
import Interpreter
import Language
import Parser
import Range
import Renderer
import Source hiding ((++))
import Syntax
import Term
import TreeSitter

import Control.Comonad.Cofree
import Control.Arrow
import Data.Bifunctor.Join
import qualified Data.ByteString.Char8 as B1
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.ICU.Detect as Detect
import qualified Data.Text.ICU.Convert as Convert
import System.FilePath

-- | Return a parser based on the file extension (including the ".").
parserForType :: T.Text -> Parser
parserForType mediaType = case languageForType mediaType of
  Just C -> treeSitterParser C ts_language_c
  Just JavaScript -> treeSitterParser JavaScript ts_language_javascript
  _ -> lineByLineParser

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser
lineByLineParser input = return . root . Indexed $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> leaves
  where
    lines = actualLines input
    root syntax = Info (Range 0 $ length input) mempty :< syntax
    leaf charIndex line = Info (Range charIndex $ charIndex + T.length line) mempty :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum ++ [ leaf charIndex (toText line) ]
      , charIndex + length line)
    toText = T.pack . Source.toString

-- | Return the parser that should be used for a given path.
parserForFilepath :: FilePath -> Parser
parserForFilepath = parserForType . T.pack . takeExtension

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

-- | Read the file and convert it to Unicode.
readAndTranscodeFile :: FilePath -> IO (Source Char)
readAndTranscodeFile path = do
  text <- B1.readFile path
  transcode text

-- | Given a parser and renderer, diff two sources and return the rendered
-- | result.
diffFiles :: Parser -> Renderer T.Text b -> (SourceBlob, SourceBlob) -> IO b
diffFiles parser renderer sourceBlobs = do
  let sources = Join $ (source *** source) sourceBlobs
  terms <- sequence $ parser <$> sources
  let replaceLeaves = breakDownLeavesByWord <$> sources
  return $ renderer (uncurry diffTerms $ runJoin $ replaceLeaves <*> terms) sourceBlobs

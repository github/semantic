module Diffing where

import Diff
import Info
import Interpreter
import Language
import Parser
import Range
import Renderer
import Source hiding ((++))
import Syntax
import Term
import TreeSitter
import Text.Parser.TreeSitter.Language

import Control.Monad.Free
import Control.Comonad.Cofree
import Data.Bifunctor.Join
import Data.Copointed
import Data.Functor.Both
import qualified Data.ByteString.Char8 as B1
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.ICU.Detect as Detect
import qualified Data.Text.ICU.Convert as Convert
import System.FilePath

-- | Return a parser based on the file extension (including the ".").
parserForType :: T.Text -> Parser
parserForType mediaType = case languageForType mediaType of
  Just C -> treeSitterParser C ts_language_c
  Just JavaScript -> treeSitterParser JavaScript ts_language_javascript
  Just Ruby -> treeSitterParser Ruby ts_language_ruby
  _ -> lineByLineParser

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser
lineByLineParser input = return . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> leaves
  where
    lines = actualLines input
    root children = Info (Range 0 $ length input) mempty (1 + fromIntegral (length children)) :< Indexed children
    leaf charIndex line = Info (Range charIndex $ charIndex + T.length line) mempty 1 :< Leaf line
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
    replaceIn (Info range categories _) (Leaf _)
      | ranges <- rangesAndWordsInSource range
      , length ranges > 1
      = Info range categories (1 + fromIntegral (length ranges)) :< Indexed (makeLeaf categories <$> ranges)
    replaceIn info syntax
      = info { size = 1 + sum (size . copoint <$> syntax) } :< syntax
    rangesAndWordsInSource range = rangesAndWordsFrom (start range) (toString $ slice range source)
    makeLeaf categories (range, substring) = Info range categories 1 :< Leaf (T.pack substring)

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
-- | Returns the rendered result strictly, so it's always fully evaluated
-- | with respect to other IO actions.
diffFiles :: Parser -> Renderer T.Text -> Both SourceBlob -> IO T.Text
diffFiles parser renderer sourceBlobs = do
  let sources = source <$> sourceBlobs
  terms <- sequence $ parser <$> sources
  let replaceLeaves = breakDownLeavesByWord <$> sources
  return $! renderer (runBothWith (diffTerms diffCostWithAbsoluteDifferenceOfCachedDiffSizes) $ replaceLeaves <*> terms) sourceBlobs

-- | The sum of the node count of the diff’s patches.
diffCostWithCachedTermSizes :: Diff a Info -> Integer
diffCostWithCachedTermSizes = diffSum (getSum . foldMap (Sum . size . copoint))

-- | The absolute difference between the node counts of a diff.
diffCostWithAbsoluteDifferenceOfCachedDiffSizes :: Diff a Info -> Integer
diffCostWithAbsoluteDifferenceOfCachedDiffSizes (Free (Annotated (Join (before, after)) _)) = abs $ size before - size after
diffCostWithAbsoluteDifferenceOfCachedDiffSizes (Pure patch) = sum $ size . copoint <$> patch

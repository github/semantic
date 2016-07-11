module Diffing where

import Prologue hiding (fst, snd)
import qualified Data.ByteString.Char8 as B1
import Data.Functor.Both
import Data.Functor.Foldable
import Data.Record
import qualified Data.Text as T
import qualified Data.Text.ICU.Detect as Detect
import qualified Data.Text.ICU.Convert as Convert
import Data.These
import Diff
import Info
import Category
import Interpreter
import Language
import Parser
import Patch
import Range
import Renderer
import Source hiding ((++))
import Syntax
import System.FilePath
import Term
import TreeSitter
import Text.Parser.TreeSitter.Language

-- | Return a parser based on the file extension (including the ".").
parserForType :: T.Text -> Parser
parserForType mediaType = case languageForType mediaType of
  Just C -> treeSitterParser C ts_language_c
  Just JavaScript -> treeSitterParser JavaScript ts_language_javascript
  Just Ruby -> treeSitterParser Ruby ts_language_ruby
  _ -> lineByLineParser

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser
lineByLineParser input = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines input
    root children = let size = 1 + fromIntegral (length children) in
      ((Range 0 $ length input) .: Other "program" .: size .: Cost (unSize size) .: RNil) :< Indexed children
    leaf charIndex line = ((Range charIndex $ charIndex + T.length line) .: Other "program" .: 1 .: 1 .: RNil) :< Leaf line
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
    replaceIn :: TermF T.Text Info (Term T.Text Info) -> Term T.Text Info
    replaceIn (info :< syntax) = let size' = 1 + sum (size . extract <$> syntax') in cofree $ setCost (setSize info size') (Cost (unSize size')) :< syntax'
      where syntax' = case (ranges, syntax) of
              (_:_:_, Leaf _) -> Indexed (makeLeaf info <$> ranges)
              _ -> syntax
            ranges = rangesAndWordsInSource (characterRange info)
    rangesAndWordsInSource range = rangesAndWordsFrom (start range) (toString $ slice range source)
    makeLeaf info (range, substring) = cofree $ setCharacterRange info range :< Leaf (T.pack substring)

-- | Transcode a file to a unicode source.
transcode :: B1.ByteString -> IO (Source Char)
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text

-- | Read the file and convert it to Unicode.
readAndTranscodeFile :: FilePath -> IO (Source Char)
readAndTranscodeFile path = do
  text <- B1.readFile path
  transcode text

-- | Given a parser and renderer, diff two sources and return the rendered
-- | result.
-- | Returns the rendered result strictly, so it's always fully evaluated
-- | with respect to other IO actions.
diffFiles :: Parser -> Renderer -> Both SourceBlob -> IO T.Text
diffFiles parser renderer sourceBlobs = do
  let sources = source <$> sourceBlobs
  terms <- sequence $ parser <$> sources

  let replaceLeaves = breakDownLeavesByWord <$> sources
  let areNullOids = runBothWith (\a b -> (oid a == nullOid || null (source a), oid b == nullOid || null (source b))) sourceBlobs
  let textDiff = case areNullOids of
        (True, False) -> pure $ Insert (snd terms)
        (False, True) -> pure $ Delete (fst terms)
        (_, _) ->
          runBothWith (diffTerms construct shouldCompareTerms diffCostWithCachedTermSizes) $ replaceLeaves <*> terms

  pure $! renderer textDiff sourceBlobs
  where construct :: CofreeF (Syntax Text) (Both Info) (Diff Text Info) -> Diff Text Info
        construct (info :< syntax) = free (Free ((setCost <$> info <*> sumCost syntax) :< syntax))
        sumCost = fmap getSum . foldMap (fmap Sum . getCost)
        getCost diff = case runFree diff of
          Free (info :< _) -> cost <$> info
          Pure patch -> uncurry both (fromThese 0 0 (unPatch (cost . extract <$> patch)))
        shouldCompareTerms = (==) `on` category . extract

-- | The sum of the node count of the diff’s patches.
diffCostWithCachedTermSizes :: Diff a Info -> Integer
diffCostWithCachedTermSizes diff = unCost $ case runFree diff of
  Free (info :< _) -> sum (cost <$> info)
  Pure patch -> sum (cost . extract <$> patch)

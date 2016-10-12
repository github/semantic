{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Diffing where

import Prologue hiding (fst, snd)
import Category
import qualified Data.ByteString.Char8 as B1
import Data.Functor.Both
import Data.RandomWalkSimilarity
import Data.Record
import qualified Data.Text.IO as TextIO
import qualified Data.Text.ICU.Detect as Detect
import qualified Data.Text.ICU.Convert as Convert
import Data.These
import qualified Data.Vector as Vector
import Diff
import Info
import Interpreter
import Language
import Language.Markdown
import Parser
import Patch
import Renderer
import Renderer.JSON
import Renderer.Patch
import Renderer.Split
import Renderer.Summary
import Source
import Syntax
import System.Directory
import System.FilePath
import qualified System.IO as IO
import System.Environment (lookupEnv)
import Term
import TreeSitter
import Text.Parser.TreeSitter.Language
import qualified Data.Text as T
import Data.Aeson (toJSON, toEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)

-- | Given a parser and renderer, diff two sources and return the rendered
-- | result.
-- | Returns the rendered result strictly, so it's always fully evaluated
-- | with respect to other IO actions.
diffFiles :: (HasField fields Category, HasField fields Cost)
          => Parser (Syntax Text) (Record fields)
          -> Renderer (Record fields)
          -> Both SourceBlob
          -> IO Output
diffFiles parser renderer sourceBlobs = do
  terms <- traverse parser sourceBlobs

  let areNullOids = runBothWith (\a b -> (oid a == nullOid || null (source a), oid b == nullOid || null (source b))) sourceBlobs
  let textDiff = case areNullOids of
        (True, False) -> pure $ Insert (snd terms)
        (False, True) -> pure $ Delete (fst terms)
        (_, _) ->
          runBothWith (diffTerms construct compareCategoryEq diffCostWithCachedTermCosts getLabel) terms

  pure $! renderer sourceBlobs textDiff

  where construct (info :< syntax) = free (Free ((setCost <$> info <*> sumCost syntax) :< syntax))
        sumCost = fmap getSum . foldMap (fmap Sum . getCost)
        getCost diff = case runFree diff of
          Free (info :< _) -> cost <$> info
          Pure patch -> uncurry both (fromThese 0 0 (unPatch (cost . extract <$> patch)))
        getLabel (h :< t) = (category h, case t of
          Leaf s -> Just s
          _ -> Nothing)

-- | Return a parser based on the file extension (including the ".").
parserForType :: Text -> Parser (Syntax Text) (Record '[Range, Category, SourceSpan])
parserForType mediaType = case languageForType mediaType of
  Just C -> treeSitterParser C ts_language_c
  Just JavaScript -> treeSitterParser JavaScript ts_language_javascript
  Just Markdown -> cmarkParser
  Just Ruby -> treeSitterParser Ruby ts_language_ruby
  _ -> lineByLineParser

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser (Syntax Text) (Record '[Range, Category, SourceSpan])
lineByLineParser SourceBlob{..} = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines source
    root children = (Range 0 (length source) .: Program .: rangeToSourceSpan source (Range 0 (length source)) .: RNil) :< Indexed children
    leaf charIndex line = (Range charIndex (charIndex + T.length line) .: Program .: rangeToSourceSpan source (Range charIndex (charIndex + T.length line)) .: RNil) :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum <> [ leaf charIndex (toText line) ] , charIndex + length line)
    toText = T.pack . Source.toString

-- | Return the parser that should be used for a given path.
parserForFilepath :: FilePath -> Parser (Syntax Text) (Record '[Cost, Range, Category, SourceSpan])
parserForFilepath path blob = decorateTerm termCostDecorator <$> parserForType (toS (takeExtension path)) blob

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

-- | A function computing a value to decorate terms with. This can be used to cache synthesized attributes on terms.
type TermDecorator f fields field = TermF f (Record fields) (Record (field ': fields)) -> field

-- | Decorate a 'Term' using a function to compute the annotation values at every node.
decorateTerm :: Functor f => TermDecorator f fields field -> Term f (Record fields) -> Term f (Record (field ': fields))
decorateTerm decorator = cata $ \ c -> cofree ((decorator (extract <$> c) .: headF c) :< tailF c)

-- | Term decorator computing the cost of an unpacked term.
termCostDecorator :: (Foldable f, Functor f) => TermDecorator f a Cost
termCostDecorator c = 1 + sum (cost <$> tailF c)

-- | Determine whether two terms are comparable based on the equality of their categories.
compareCategoryEq :: Functor f => HasField fields Category => Term f (Record fields) -> Term f (Record fields) -> Bool
compareCategoryEq = (==) `on` category . extract

-- | The sum of the node count of the diff’s patches.
diffCostWithCachedTermCosts :: Functor f => HasField fields Cost => Diff f (Record fields) -> Int
diffCostWithCachedTermCosts diff = unCost $ case runFree diff of
  Free (info :< _) -> sum (cost <$> info)
  Pure patch -> sum (cost . extract <$> patch)

-- | Returns a rendered diff given a parser, diff arguments and two source blobs.
textDiff :: (HasField fields SourceSpan, HasField fields Category, HasField fields Cost, HasField fields Range) => Parser (Syntax Text) (Record fields) -> DiffArguments -> Both SourceBlob -> IO Output
textDiff parser arguments = diffFiles parser $ case format arguments of
  Split -> split
  Patch -> patch
  JSON -> json
  Summary -> summary

-- | Returns a truncated diff given diff arguments and two source blobs.
truncatedDiff :: DiffArguments -> Both SourceBlob -> IO Output
truncatedDiff arguments sources = pure $ case format arguments of
  Split -> SplitOutput mempty
  Patch -> PatchOutput (truncatePatch arguments sources)
  JSON -> JSONOutput mempty
  Summary -> SummaryOutput mempty

-- | Prints a rendered diff to stdio or a filepath given a parser, diff arguments and two source blobs.
printDiff :: (HasField fields SourceSpan, HasField fields Category, HasField fields Cost, HasField fields Range) => Parser (Syntax Text) (Record fields) -> DiffArguments -> Both SourceBlob -> IO ()
printDiff parser arguments sources = do
  rendered <- textDiff parser arguments sources
  let renderedText = case rendered of
                       SplitOutput text -> text
                       PatchOutput text -> text
                       JSONOutput series -> toS . encodingToLazyByteString . toEncoding $ toJSON series
                       SummaryOutput summaries -> toS . encodingToLazyByteString . toEncoding $ toJSON summaries

  writeToOutput (output arguments) renderedText

-- | Writes text to an output file or stdout.
writeToOutput :: Maybe FilePath -> Text -> IO ()
writeToOutput output text =
  case output of
    Nothing -> do
      lang <- lookupEnv "LANG"
      if isNothing lang then IO.hSetEncoding IO.stdout IO.utf8 else pure ()
      TextIO.hPutStrLn IO.stdout text
    Just path -> do
      isDir <- doesDirectoryExist path
      let outputPath = if isDir
          then path </> (takeFileName outputPath -<.> ".html")
          else path
      IO.withFile outputPath IO.WriteMode (`TextIO.hPutStr` text)

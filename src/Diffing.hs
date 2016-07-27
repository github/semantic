{-# LANGUAGE DataKinds, TypeOperators #-}
module Diffing where

import Prologue hiding (fst, snd)
import qualified Data.ByteString.Char8 as B1
import Data.Functor.Both
import Data.Functor.Foldable
import Data.Record
import qualified Data.Text.IO as TextIO
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
import Renderer.JSON
import Renderer.Patch
import Renderer.Split
import Renderer.Summary
import Source hiding ((++))
import Syntax
import System.Directory
import System.FilePath
import qualified System.IO as IO
import Term
import TreeSitter
import Text.Parser.TreeSitter.Language
import qualified Data.Text as T

-- | Return a parser based on the file extension (including the ".").
parserForType :: Text -> Parser (Syntax Text) (Record '[Range, Category])
parserForType mediaType = case languageForType mediaType of
  Just C -> treeSitterParser C ts_language_c
  Just JavaScript -> treeSitterParser JavaScript ts_language_javascript
  Just Ruby -> treeSitterParser Ruby ts_language_ruby
  _ -> lineByLineParser

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser (Syntax Text) (Record '[Range, Category])
lineByLineParser input = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines input
    root children = ((Range 0 $ length input) .: Other "program" .: RNil) :< Indexed children
    leaf charIndex line = ((Range charIndex $ charIndex + T.length line) .: Other "program" .: RNil) :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum <> [ leaf charIndex (toText line) ] , charIndex + length line)
    toText = T.pack . Source.toString

-- | Return the parser that should be used for a given path.
parserForFilepath :: FilePath -> Parser (Syntax Text) (Record '[Range, Category])
parserForFilepath path source = do
   parsed <- parserForType (toS (takeExtension path)) source
   pure $! breakDownLeavesByWord source parsed

-- | Replace every string leaf with leaves of the words in the string.
breakDownLeavesByWord :: (HasField fields Category, HasField fields Range) => Source Char -> Term Text (Record fields) -> Term Text (Record fields)
breakDownLeavesByWord source = cata replaceIn
  where
    replaceIn (info :< syntax) = cofree $ info :< syntax'
      where syntax' = case (ranges, syntax) of
              (_:_:_, Leaf _) | category info /= Regex -> Indexed (makeLeaf info <$> ranges)
              _ -> syntax
            ranges = rangesAndWordsInSource (characterRange info)
    rangesAndWordsInSource range = rangesAndWordsFrom (start range) (toString $ slice range source)
    makeLeaf info (range, substring) = cofree $ setCharacterRange info range :< Leaf (toS substring)

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
type TermDecorator f fields field = CofreeF f (Record fields) (Record (field ': fields)) -> field

-- | Decorate the 'Term's produced by a 'Parser' using a function to compute the annotation values at every node.
decorateParser :: (Typeable field, Functor f) => TermDecorator f fields field -> Parser f (Record fields) -> Parser f (Record (field ': fields))
decorateParser decorator = (fmap (decorateTerm decorator) .)

-- | Decorate a 'Term' using a function to compute the annotation values at every node.
decorateTerm :: (Typeable field, Functor f) => TermDecorator f fields field -> Cofree f (Record fields) -> Cofree f (Record (field ': fields))
decorateTerm decorator = cata $ \ c -> cofree ((decorator (extract <$> c) .: headF c) :< tailF c)

-- | Given a parser and renderer, diff two sources and return the rendered
-- | result.
-- | Returns the rendered result strictly, so it's always fully evaluated
-- | with respect to other IO actions.
diffFiles :: (HasField fields Category, HasField fields Cost, HasField fields Range, Eq (Record fields)) => Parser (Syntax Text) (Record fields) -> Renderer (Record fields) -> Both SourceBlob -> IO Text
diffFiles parser renderer sourceBlobs = do
  terms <- traverse (parser . source) sourceBlobs

  let areNullOids = runJoin $ (== nullOid) . oid <$> sourceBlobs
  let textDiff = case areNullOids of
        (True, False) -> pure $ Insert (snd terms)
        (False, True) -> pure $ Delete (fst terms)
        _ -> runBothWith (diffTerms construct compareCategoryEq diffCostWithCachedTermCosts) terms

  pure $! renderer textDiff sourceBlobs

  where construct (info :< syntax) = free (Free ((setCost <$> info <*> sumCost syntax) :< syntax))
        sumCost = fmap getSum . foldMap (fmap Sum . getCost)
        getCost diff = case runFree diff of
          Free (info :< _) -> cost <$> info
          Pure patch -> uncurry both (fromThese 0 0 (unPatch (cost . extract <$> patch)))

compareCategoryEq :: HasField fields Category => Term leaf (Record fields) -> Term leaf (Record fields) -> Bool
compareCategoryEq = (==) `on` category . extract

-- | Term decorator computing the cost of an unpacked term.
termCostDecorator :: (Prologue.Foldable f, Functor f) => TermDecorator f a Cost
termCostDecorator c = 1 + sum (cost <$> tailF c)

-- | The sum of the node count of the diff’s patches.
diffCostWithCachedTermCosts :: HasField fields Cost => Diff leaf (Record fields) -> Integer
diffCostWithCachedTermCosts diff = unCost $ case runFree diff of
  Free (info :< _) -> sum (cost <$> info)
  Pure patch -> sum (cost . extract <$> patch)


-- | Returns a rendered diff given a parser, diff arguments and two source blobs.
textDiff :: (Eq (Record fields), HasField fields Category, HasField fields Cost, HasField fields Range) => Parser (Syntax Text) (Record fields) -> DiffArguments -> Both SourceBlob -> IO Text
textDiff parser arguments = diffFiles parser $ case format arguments of
  Split -> split
  Patch -> patch
  JSON -> json
  Summary -> summary

-- | Returns a truncated diff given diff arguments and two source blobs.
truncatedDiff :: DiffArguments -> Both SourceBlob -> IO Text
truncatedDiff arguments sources = case format arguments of
  Split -> pure ""
  Patch -> pure $ truncatePatch arguments sources
  JSON -> pure "{}"
  Summary -> pure ""

-- | Prints a rendered diff to stdio or a filepath given a parser, diff arguments and two source blobs.
printDiff :: (Eq (Record fields), HasField fields Category, HasField fields Cost, HasField fields Range) => Parser (Syntax Text) (Record fields) -> DiffArguments -> Both SourceBlob -> IO ()
printDiff parser arguments sources = do
  rendered <- textDiff parser arguments sources
  case (output arguments) of
    Nothing -> TextIO.putStr rendered
    Just path -> do
      isDir <- doesDirectoryExist path
      let outputPath = if isDir
          then path </> (takeFileName outputPath -<.> ".html")
          else path
      IO.withFile outputPath IO.WriteMode (`TextIO.hPutStr` rendered)

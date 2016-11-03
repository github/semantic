{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Parse where

import Arguments
import Category
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8 as B1
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect
import Data.Record
import qualified Data.Text as T
import Info
import Language
import Language.Markdown
import Parser
import Prologue
import Source
import Syntax
import System.FilePath
import Term
import TreeSitter
import Text.Parser.TreeSitter.Language
import Renderer.JSON()

run :: Arguments -> IO ()
run args@Arguments{..} = do
  sources <- sequence $ readAndTranscodeFile <$> filePaths

  let sourceBlobs = Source.SourceBlob <$> sources <*> pure mempty <*> filePaths <*> pure (Just Source.defaultPlainBlob)
  let parsers = parserWithCost <$> filePaths
  let parsersAndBlobs = zip parsers sourceBlobs

  terms <- traverse (\(parser, sourceBlob) -> parser sourceBlob) parsersAndBlobs

  putStrLn $ encodePretty terms

  pure ()

-- | Return a parser that decorates with the cost of a term and its children.
parserWithCost :: FilePath -> Parser (Syntax Text) (Record '[Cost, Range, Category, SourceSpan])
parserWithCost path blob = decorateTerm termCostDecorator <$> parserForType (toS (takeExtension path)) blob

-- | Return a parser based on the file extension (including the ".").
parserForType :: Text -> Parser (Syntax Text) (Record '[Range, Category, SourceSpan])
parserForType mediaType = case languageForType mediaType of
  Just C -> treeSitterParser C ts_language_c
  Just JavaScript -> treeSitterParser JavaScript ts_language_javascript
  Just Markdown -> cmarkParser
  Just Ruby -> treeSitterParser Ruby ts_language_ruby
  _ -> lineByLineParser

-- | Decorate a 'Term' using a function to compute the annotation values at every node.
decorateTerm :: Functor f => TermDecorator f fields field -> Term f (Record fields) -> Term f (Record (field ': fields))
decorateTerm decorator = cata $ \ c -> cofree ((decorator (extract <$> c) .: headF c) :< tailF c)

-- | A function computing a value to decorate terms with. This can be used to cache synthesized attributes on terms.
type TermDecorator f fields field = TermF f (Record fields) (Record (field ': fields)) -> field

-- | Term decorator computing the cost of an unpacked term.
termCostDecorator :: (Foldable f, Functor f) => TermDecorator f a Cost
termCostDecorator c = 1 + sum (cost <$> tailF c)

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

-- | Read the file and convert it to Unicode.
readAndTranscodeFile :: FilePath -> IO (Source Char)
readAndTranscodeFile path = do
  text <- B1.readFile path
  transcode text

-- | Transcode a file to a unicode source.
transcode :: B1.ByteString -> IO (Source Char)
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text

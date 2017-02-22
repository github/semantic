{-# LANGUAGE DataKinds, RankNTypes, TypeOperators, DeriveAnyClass #-}
module Parse where

import Arguments
import Category
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B1
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect
import Data.Record
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Renderer
import Renderer.JSON()
import Renderer.SExpression
import Text.Parser.TreeSitter.C
import Text.Parser.TreeSitter.Go
import Text.Parser.TreeSitter.JavaScript
import Text.Parser.TreeSitter.Ruby

data ParseJSON = ParseJSON
  { category :: Text
  , range :: Range
  , text :: SourceText
  , children :: [ParseJSON]
  } deriving (Show, Generic, ToJSON)

run :: Arguments -> IO ()
run Arguments{..} = do
  sources <- mapM readAndTranscodeFile filePaths
  terms <- zipWithM (\parser sourceBlob -> parser sourceBlob) parsers (sourceBlobs sources)

  writeToOutput output $ case format of
    SExpression -> [foldr (\t acc -> printTerm t 0 TreeOnly <> acc) "" terms]
    _ -> toS . encodePretty . cata algebra <$> terms

  where
    sourceBlobs sources = Source.SourceBlob <$> sources <*> pure mempty <*> filePaths <*> pure (Just Source.defaultPlainBlob)
    parsers = parserWithSource <$> filePaths

    algebra :: TermF (Syntax leaf) (Record '[SourceText, Range, Category, SourceSpan]) ParseJSON -> ParseJSON
    algebra term = case term of
      (annotation :< Leaf _) -> ParseJSON (category' annotation) (range' annotation) (text' annotation) []
      (annotation :< syntax) -> ParseJSON (category' annotation) (range' annotation) (text' annotation) (toList syntax)
      where
        category' = toS . Info.category
        range' = byteRange
        text' = Info.sourceText

    writeToOutput :: Maybe FilePath -> [Text] -> IO ()
    writeToOutput output text =
      case output of
        Nothing -> for_ text putStrLn
        Just path -> for_ text (T.writeFile path)


-- | Return a parser that decorates with the source text.
parserWithSource :: FilePath -> Parser (Syntax Text) (Record '[SourceText, Range, Category, SourceSpan])
parserWithSource path blob = decorateTerm (termSourceDecorator (source blob)) <$> parserForType (toS (takeExtension path)) blob

-- | Return a parser based on the file extension (including the ".").
parserForType :: Text -> Parser (Syntax Text) (Record '[Range, Category, SourceSpan])
parserForType mediaType = case languageForType mediaType of
  Just C -> treeSitterParser C tree_sitter_c
  Just JavaScript -> treeSitterParser JavaScript tree_sitter_javascript
  Just Markdown -> cmarkParser
  Just Ruby -> treeSitterParser Ruby tree_sitter_ruby
  Just Language.Go -> treeSitterParser Language.Go tree_sitter_go
  _ -> lineByLineParser

-- | Decorate a 'Term' using a function to compute the annotation values at every node.
decorateTerm :: (Functor f) => TermDecorator f fields field -> Term f (Record fields) -> Term f (Record (field ': fields))
decorateTerm decorator = cata $ \ term -> cofree ((decorator (extract <$> term) :. headF term) :< tailF term)

-- | A function computing a value to decorate terms with. This can be used to cache synthesized attributes on terms.
type TermDecorator f fields field = TermF f (Record fields) (Record (field ': fields)) -> field

-- | Term decorator extracting the source text for a term.
termSourceDecorator :: (HasField fields Range) => Source -> TermDecorator f fields SourceText
termSourceDecorator source c = SourceText . toText $ Source.slice range' source
 where range' = byteRange $ headF c

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser (Syntax Text) (Record '[Range, Category, SourceSpan])
lineByLineParser SourceBlob{..} = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines source
    root children = (sourceRange :. Program :. rangeToSourceSpan source sourceRange :. Nil) :< Indexed children
    sourceRange = Source.totalRange source
    leaf charIndex line = (Range charIndex (charIndex + T.length line) :. Program :. rangeToSourceSpan source (Range charIndex (charIndex + T.length line)) :. Nil) :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum <> [ leaf charIndex (Source.toText line) ] , charIndex + Source.length line)

-- | Return the parser that should be used for a given path.
parserForFilepath :: FilePath -> Parser (Syntax Text) (Record '[Range, Category, SourceSpan])
parserForFilepath = parserForType . toS . takeExtension

-- | Read the file and convert it to Unicode.
readAndTranscodeFile :: FilePath -> IO Source
readAndTranscodeFile path = do
  text <- B1.readFile path
  transcode text

-- | Transcode a file to a unicode source.
transcode :: B1.ByteString -> IO Source
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  pure $ Convert.toUnicode converter text

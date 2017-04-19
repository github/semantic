module Parser.Language
( parserForLanguage
, parserForFilePath -- TODO: Remove. Langauge detection should not come from file extension.
) where

import Data.Record
import qualified Data.Text as T
import Parser
import Source
import Prologue
import Info
import Language
import Language.Markdown
import Syntax
import TreeSitter
import Text.Parser.TreeSitter.C
import Text.Parser.TreeSitter.Go
import Text.Parser.TreeSitter.JavaScript
import Text.Parser.TreeSitter.Ruby
import Text.Parser.TreeSitter.TypeScript

-- TODO: Shouldn't need to depend on System.FilePath in here
import System.FilePath

  -- | Return a parser for a given langauge or the lineByLineParser parser.
parserForLanguage :: Maybe Language -> Parser (Syntax Text) (Record DefaultFields)
parserForLanguage Nothing = lineByLineParser
parserForLanguage (Just language) = case language of
  C -> treeSitterParser C tree_sitter_c
  JavaScript -> treeSitterParser JavaScript tree_sitter_javascript
  TypeScript -> treeSitterParser TypeScript tree_sitter_typescript
  Markdown -> cmarkParser
  Ruby -> treeSitterParser Ruby tree_sitter_ruby
  Language.Go -> treeSitterParser Language.Go tree_sitter_go

-- | Return a parser based on the FilePath's extension (including the ".").
-- | TODO: Remove.
parserForFilePath :: FilePath -> Parser (Syntax Text) (Record DefaultFields)
parserForFilePath = parserForLanguage . languageForType . toS . takeExtension

-- | A fallback parser that treats a file simply as rows of strings.
lineByLineParser :: Parser (Syntax Text) (Record DefaultFields)
lineByLineParser SourceBlob{..} = pure . cofree . root $ case foldl' annotateLeaves ([], 0) lines of
  (leaves, _) -> cofree <$> leaves
  where
    lines = actualLines source
    root children = (sourceRange :. Program :. rangeToSourceSpan source sourceRange :. Nil) :< Indexed children
    sourceRange = Source.totalRange source
    leaf charIndex line = (Range charIndex (charIndex + T.length line) :. Program :. rangeToSourceSpan source (Range charIndex (charIndex + T.length line)) :. Nil) :< Leaf line
    annotateLeaves (accum, charIndex) line =
      (accum <> [ leaf charIndex (Source.toText line) ] , charIndex + Source.length line)

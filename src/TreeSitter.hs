{-# LANGUAGE DataKinds #-}
module TreeSitter (treeSitterParser) where

import Prologue hiding (Constructor)
import Category
import Data.Record
import Language
import qualified Language.C as C
import qualified Language.Go as Go
import qualified Language.JavaScript as JS
import qualified Language.Ruby as Ruby
import Parser
import Range
import Source
import qualified Syntax
import Foreign
import Foreign.C.String
import Text.Parser.TreeSitter hiding (Language(..))
import qualified Text.Parser.TreeSitter as TS
import SourceSpan
import Info

-- | Returns a TreeSitter parser for the given language and TreeSitter grammar.
treeSitterParser :: Language -> Ptr TS.Language -> Parser (Syntax.Syntax Text) (Record '[Range, Category, SourceSpan])
treeSitterParser language grammar blob = do
  document <- ts_document_new
  ts_document_set_language document grammar
  withCString (toString $ source blob) (\source -> do
    ts_document_set_input_string document source
    ts_document_parse document
    term <- documentToTerm language document blob
    ts_document_free document
    pure term)

-- | Return a parser for a tree sitter language & document.
documentToTerm :: Language -> Ptr Document -> Parser (Syntax.Syntax Text) (Record '[Range, Category, SourceSpan])
documentToTerm language document SourceBlob{..} = alloca $ \ root -> do
  ts_document_root_node_p document root
  toTerm root
  where toTerm node = do
          name <- ts_node_p_name node document
          name <- peekCString name
          count <- ts_node_p_named_child_count node
          children <- filter isNonEmpty <$> traverse (alloca . getChild node) (take (fromIntegral count) [0..])

          let range = Range { start = fromIntegral $ ts_node_p_start_char node, end = fromIntegral $ ts_node_p_end_char node }

          let startPos = SourcePos (1 + (fromIntegral $! ts_node_p_start_point_row node)) (1 + (fromIntegral $! ts_node_p_start_point_column node))
          let endPos = SourcePos (1 + (fromIntegral $! ts_node_p_end_point_row node)) (1 + (fromIntegral $! ts_node_p_end_point_column node))
          let sourceSpan = SourceSpan { spanStart = startPos , spanEnd = endPos }

          allChildrenCount <- ts_node_p_child_count node
          let allChildren = filter isNonEmpty <$> traverse (alloca . getUnnamedChild node) (take (fromIntegral allChildrenCount) [0..])

          -- Note: The strict application here is semantically important.
          -- Without it, we may not evaluate the range until after we’ve exited
          -- the scope that `node` was allocated within, meaning `alloca` will
          -- free it & other stack data may overwrite it.
          range `seq` termConstructor source (pure $! sourceSpan) (toS name) range children allChildren
        getChild node n out = ts_node_p_named_child node n out >> toTerm out
        {-# INLINE getChild #-}
        getUnnamedChild node n out = ts_node_p_child node n out >> toTerm out
        {-# INLINE getUnnamedChild #-}
        termConstructor = case language of
          JavaScript -> JS.termConstructor
          C -> C.termConstructor
          Language.Go -> Go.termConstructor
          Ruby -> Ruby.termConstructor
          _ -> Language.termConstructor
        isNonEmpty child = category (extract child) /= Empty

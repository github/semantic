module TreeSitter where

import Prologue hiding (Constructor)
import Data.String
import Category
import Language
import Parser
import Range
import Source
import qualified Data.Set as Set
import Foreign
import Foreign.C.String
import Text.Parser.TreeSitter hiding (Language(..))
import qualified Text.Parser.TreeSitter as TS

-- | Returns a TreeSitter parser for the given language and TreeSitter grammar.
treeSitterParser :: Language -> Ptr TS.Language -> Parser
treeSitterParser language grammar contents = do
  document <- ts_document_make
  ts_document_set_language document grammar
  withCString (toString contents) (\source -> do
    ts_document_set_input_string document source
    ts_document_parse document
    term <- documentToTerm (termConstructor $ categoriesForLanguage language) document contents
    ts_document_free document
    pure term)

-- Given a language and a node name, return the correct categories.
categoriesForLanguage :: Language -> String -> Category
categoriesForLanguage language name = case (language, name) of
  (JavaScript, "object") -> DictionaryLiteral
  (JavaScript, "rel_op") -> BinaryOperator -- relational operator, e.g. >, <, <=, >=, ==, !=

  (Ruby, "hash") -> DictionaryLiteral
  _ -> defaultCategoryForNodeName name

-- | Given a node name from TreeSitter, return the correct categories.
defaultCategoryForNodeName :: String -> Category
defaultCategoryForNodeName name = case name of
  "function_call" -> FunctionCall
  "pair" -> Pair
  "string" -> StringLiteral
  "integer" -> IntegerLiteral
  "symbol" -> SymbolLiteral
  "array" -> ArrayLiteral
  _ -> (Other name)

-- | Given a constructor and a tree sitter document, return a parser.
documentToTerm :: Constructor -> Ptr Document -> Parser
documentToTerm constructor document contents = alloca $ \ root -> do
  ts_document_root_node_p document root
  toTerm root
  where toTerm node = do
          name <- ts_node_p_name node document
          name <- peekCString name
          count <- ts_node_p_named_child_count node
          children <- traverse (alloca . getChild node) $ take (fromIntegral count) [0..]
          -- Note: The strict application here is semantically important. Without it, we may not evaluate the range until after we’ve exited the scope that `node` was allocated within, meaning `alloca` will free it & other stack data may overwrite it.
          range <- pure $! Range { start = fromIntegral $ ts_node_p_start_char node, end = fromIntegral $ ts_node_p_end_char node }

          pure $! constructor contents range name children
        getChild node n out = do
          _ <- ts_node_p_named_child node n out
          toTerm out

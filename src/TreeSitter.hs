module TreeSitter where

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
    return term)

-- Given a language and a node name, return the correct categories.
categoriesForLanguage :: Language -> String -> Set.Set Category
categoriesForLanguage language name = case (language, name) of
  (JavaScript, "object") -> Set.singleton DictionaryLiteral
  (JavaScript, "rel_op") -> Set.singleton BinaryOperator -- relational operator, e.g. >, <, <=, >=, ==, !=

  (Ruby, "hash") -> Set.singleton DictionaryLiteral
  _ -> defaultCategoryForNodeName name

-- | Given a node name from TreeSitter, return the correct categories.
defaultCategoryForNodeName :: String -> Set.Set Category
defaultCategoryForNodeName name = case name of
  "function_call" -> Set.singleton FunctionCall
  "pair" -> Set.singleton Pair
  "string" -> Set.singleton StringLiteral
  "integer" -> Set.singleton IntegerLiteral
  "symbol" -> Set.singleton SymbolLiteral
  "array" -> Set.singleton ArrayLiteral
  _ -> Set.singleton (Other name)

-- | Given a constructor and a tree sitter document, return a parser.
documentToTerm :: Constructor -> Ptr Document -> Parser
documentToTerm constructor document contents = alloca $ \ root -> do
  ts_document_root_node_p document root
  toTerm root
  where toTerm node = do
          name <- ts_node_p_name node document
          name <- peekCString name
          count <- ts_node_p_named_child_count node
          children <- mapM (alloca . getChild node) $ take (fromIntegral count) [0..]
          -- Note: The strict application here is semantically important. Without it, we may not evaluate the range until after we’ve exited the scope that `node` was allocated within, meaning `alloca` will free it & other stack data may overwrite it.
          range <- return $! Range { start = fromIntegral $ ts_node_p_start_char node, end = fromIntegral $ ts_node_p_end_char node }

          return $! constructor contents range name children
        getChild node n out = do
          _ <- ts_node_p_named_child node n out
          toTerm out

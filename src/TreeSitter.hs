{-# LANGUAGE DataKinds #-}
module TreeSitter where

import Prologue hiding (Constructor)
import Data.Record
import Category
import Info
import Language
import Parser
import Range
import Source
import Foreign
import Foreign.C.String
import Text.Parser.TreeSitter hiding (Language(..))
import qualified Text.Parser.TreeSitter as TS

-- | Returns a TreeSitter parser for the given language and TreeSitter grammar.
treeSitterParser :: Language -> Ptr TS.Language -> Parser '[Range, Category, Cost]
treeSitterParser language grammar contents = do
  document <- ts_document_make
  ts_document_set_language document grammar
  withCString (toString contents) (\source -> do
    ts_document_set_input_string document source
    ts_document_parse document
    term <- documentToTerm language document contents
    ts_document_free document
    pure term)

-- Given a language and a node name, return the correct categories.
categoriesForLanguage :: Language -> Text -> Category
categoriesForLanguage language name = case (language, name) of
  (JavaScript, "object") -> Object
  (JavaScript, "rel_op") -> BinaryOperator -- relational operator, e.g. >, <, <=, >=, ==, !=
  (JavaScript, "this_expression") -> Identifier
  (JavaScript, "null") -> Identifier
  (JavaScript, "undefined") -> Identifier
  (JavaScript, "arrow_function") -> Function
  (JavaScript, "generator_function") -> Function
  (JavaScript, "delete_op") -> Operator
  (JavaScript, "type_op") -> Operator
  (JavaScript, "void_op") -> Operator
  (JavaScript, "for_in_statement") -> For
  (JavaScript, "for_of_statement") -> For

  (Ruby, "hash") -> Object
  _ -> defaultCategoryForNodeName name

-- | Given a node name from TreeSitter, return the correct categories.
defaultCategoryForNodeName :: Text -> Category
defaultCategoryForNodeName name = case name of
  "program" -> Program
  "ERROR" -> Error
  "function_call" -> FunctionCall
  "pair" -> Pair
  "string" -> StringLiteral
  "integer" -> IntegerLiteral
  "symbol" -> SymbolLiteral
  "array" -> ArrayLiteral
  "function" -> Function
  "identifier" -> Identifier
  "formal_parameters" -> Params
  "arguments" -> Args
  "statement_block" -> ExpressionStatements
  "assignment" -> Assignment
  "member_access" -> MemberAccess
  "op" -> Operator
  "subscript_access" -> SubscriptAccess
  "regex" -> Regex
  "template_string" -> TemplateString
  "var_assignment" -> VarAssignment
  "var_declaration" -> VarDecl
  "switch_statement" -> Switch
  "math_assignment" -> MathAssignment
  "case" -> Case
  "true" -> Boolean
  "false" -> Boolean
  "ternary" -> Ternary
  "for_statement" -> For
  "while_statement" -> While
  "do_statement" -> DoWhile
  "return_statement" -> Return
  _ -> Other name

-- | Return a parser for a tree sitter language & document.
documentToTerm :: Language -> Ptr Document -> Parser '[Range, Category, Cost]
documentToTerm language document contents = alloca $ \ root -> do
  ts_document_root_node_p document root
  toTerm root
  where toTerm node = do
          name <- ts_node_p_name node document
          name <- peekCString name
          count <- ts_node_p_named_child_count node
          children <- traverse (alloca . getChild node) $ take (fromIntegral count) [0..]
          -- Note: The strict application here is semantically important. Without it, we may not evaluate the range until after we’ve exited the scope that `node` was allocated within, meaning `alloca` will free it & other stack data may overwrite it.
          range <- pure $! Range { start = fromIntegral $ ts_node_p_start_char node, end = fromIntegral $ ts_node_p_end_char node }

          let cost' = 1 + sum (cost . extract <$> children)
          let info = range .: (categoriesForLanguage language (toS name)) .: cost' .: RNil
          pure $! termConstructor contents info children
        getChild node n out = do
          _ <- ts_node_p_named_child node n out
          toTerm out

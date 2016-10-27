{-# LANGUAGE DataKinds #-}
module Language.Ruby where

import Data.Record
import Info
import Prologue
import Source
import Language
import qualified Syntax as S
import Term

operators :: [Text]
operators = ["and", "boolean_and", "or", "boolean_or", "bitwise_or", "bitwise_and", "shift", "relational", "comparison"]

functions :: [Text]
functions = [ "lambda_literal", "lambda_expression" ]

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])] -- ^ The child nodes of the term.
  -> IO (Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children
  | name == "ERROR" = withDefaultInfo (S.Error children)
  | otherwise = withDefaultInfo $ case (name, children) of
    ("argument_list", _) -> S.Args children
    ("array", _) -> S.Array children
    ("assignment", [ identifier, value ]) -> S.Assignment identifier value
    ("case_statement", expr : rest) -> S.Switch expr rest
    ("class_declaration", [ identifier, superclass, definitions ]) -> S.Class identifier (Just superclass) (toList (unwrap definitions))
    ("class_declaration", [ identifier, definitions ]) -> S.Class identifier Nothing (toList (unwrap definitions))
    ("comment", _) -> S.Comment . toText $ slice range source
    ("conditional_assignment", [ identifier, value ]) -> S.ConditionalAssignment identifier value
    ("conditional", condition : cases) -> S.Ternary condition cases
    ("function_call", _) -> case runCofree <$> children of
      [ _ :< S.MemberAccess{..}, _ :< S.Args args ] -> S.MethodCall memberId property args
      [ _ :< S.MemberAccess{..} ] -> S.MethodCall memberId property []
      [ function, _ :< S.Args args ] -> S.FunctionCall (cofree function) args
      (x:xs) -> S.FunctionCall (cofree x) (cofree <$> xs)
      _ -> S.Indexed children
    ("hash", _) -> S.Object $ foldMap toTuple children
    ("if_modifier", [ lhs, condition ]) -> S.If condition [lhs]
    ("if_statement", expr : rest ) -> S.If expr rest
    ("math_assignment", [ identifier, value ]) -> S.MathAssignment identifier value
    ("member_access", [ base, property ]) -> S.MemberAccess base property
    ("method_declaration", [ identifier, params, exprs ]) -> S.Method identifier (toList (unwrap params)) (toList (unwrap exprs))
    ("method_declaration", [ identifier, exprs ]) -> S.Method identifier [] (toList (unwrap exprs))
    ("module_declaration", identifier : body ) -> S.Module identifier body
    ("return_statement", _) -> S.Return (listToMaybe children)
    ("unless_modifier", [ lhs, condition ]) -> S.Unless condition [lhs]
    ("unless_statement", expr : rest ) -> S.Unless expr rest
    ("until_modifier", [ lhs, condition ]) -> S.Until condition (Just lhs)
    ("until_statement", [ expr, body ]) -> S.Until expr (Just body)
    ("until_statement", [ expr ]) -> S.Until expr Nothing
    ("while_modifier", [ lhs, condition ]) -> S.While condition (Just lhs)
    ("while_statement", [ expr, body ]) -> S.While expr (Just body)
    ("while_statement", [ expr ]) -> S.While expr Nothing
    ("yield", _) -> S.Yield (listToMaybe children)
    ("for_statement", lhs : expr : rest ) -> S.For [lhs, expr] rest
    _ | name `elem` operators -> S.Operator children
    _ | name `elem` functions -> case children of
          [ body ] -> S.AnonymousFunction [] [body]
          ( params : body ) -> S.AnonymousFunction (toList (unwrap params)) body
          _ -> S.Indexed children
    (_, []) -> S.Leaf . toText $ slice range source
    _  -> S.Indexed children
  where
    withDefaultInfo syntax = do
      sourceSpan' <- sourceSpan
      pure $! cofree ((range .: categoryForRubyName name .: sourceSpan' .: RNil) :< syntax)

categoryForRubyName :: Text -> Category
categoryForRubyName = \case
  "and" -> BooleanOperator
  "argument_list" -> Args
  "array" -> ArrayLiteral
  "assignment" -> Assignment
  "bitwise_and" -> BitwiseOperator -- bitwise and, e.g &.
  "bitwise_or" -> BitwiseOperator -- bitwise or, e.g. ^, |.
  "boolean_and" -> BooleanOperator -- boolean and, e.g. &&.
  "boolean_or" -> BooleanOperator -- boolean or, e.g. &&.
  "boolean" -> Boolean
  "case_statement" -> Switch
  "class_declaration"  -> Class
  "comment" -> Comment
  "comparison" -> RelationalOperator -- comparison operator, e.g. <, <=, >=, >.
  "conditional_assignment" -> ConditionalAssignment
  "conditional" -> Ternary
  "ERROR" -> Error
  "float" -> NumberLiteral
  "for_statement" -> For
  "formal_parameters" -> Params
  "function_call" -> FunctionCall
  "function" -> Function
  "hash" -> Object
  "identifier" -> Identifier
  "if_modifier" -> If
  "if_statement" -> If
  "integer" -> IntegerLiteral
  "interpolation" -> Interpolation
  "math_assignment" -> MathAssignment
  "member_access" -> MemberAccess
  "method_declaration" -> Method
  "module_declaration"  -> Module
  "nil" -> Identifier
  "or" -> BooleanOperator
  "program" -> Program
  "regex" -> Regex
  "relational" -> RelationalOperator -- relational operator, e.g. ==, !=, ===, <=>, =~, !~.
  "return_statement" -> Return
  "shift" -> BitwiseOperator -- bitwise shift, e.g <<, >>.
  "string" -> StringLiteral
  "subshell" -> Subshell
  "symbol" -> SymbolLiteral
  "unless_modifier" -> Unless
  "unless_statement" -> Unless
  "until_modifier" -> Until
  "until_statement" -> Until
  "while_modifier" -> While
  "while_statement" -> While
  "yield" -> Yield
  s -> Other s

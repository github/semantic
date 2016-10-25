{-# LANGUAGE DataKinds #-}
module Language.Ruby where

import Data.Record
import Info
import Prologue
import Source
import Language
import qualified Syntax as S
import Term

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
    ("math_assignment", [ identifier, value ]) -> S.MathAssignment identifier value
    ("member_access", [ base, property ]) -> S.MemberAccess base property
    ("method_declaration", [ identifier, params, exprs ]) -> S.Method identifier (toList (unwrap params)) (toList (unwrap exprs))
    ("method_declaration", [ identifier, exprs ]) -> S.Method identifier [] (toList (unwrap exprs))
    ("return_statement", _) -> S.Return (listToMaybe children)
    _ | name `elem` ["boolean_and", "boolean_or", "bitwise_or", "bitwise_and", "shift", "relational", "comparison"]
      -> S.Operator children
    (_, []) -> S.Leaf . toText $ slice range source
    _  -> S.Indexed children
  where
    withDefaultInfo syntax = do
      sourceSpan' <- sourceSpan
      pure $! cofree ((range .: categoryForRubyName name .: sourceSpan' .: RNil) :< syntax)

categoryForRubyName :: Text -> Category
categoryForRubyName = \case
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
  "formal_parameters" -> Params
  "function_call" -> FunctionCall
  "hash" -> Object
  "identifier" -> Identifier
  "integer" -> IntegerLiteral
  "interpolation" -> Interpolation
  "math_assignment" -> MathAssignment
  "member_access" -> MemberAccess
  "method_declaration" -> Method
  "nil" -> Identifier
  "program" -> Program
  "relational" -> RelationalOperator -- relational operator, e.g. ==, !=, ===, <=>, =~, !~.
  "return_statement" -> Return
  "shift" -> BitwiseOperator -- bitwise shift, e.g <<, >>.
  "string" -> StringLiteral
  "subshell" -> Subshell
  "symbol" -> SymbolLiteral
  s -> Other s

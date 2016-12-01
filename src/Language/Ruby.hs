{-# LANGUAGE DataKinds #-}
module Language.Ruby where

import Data.Record
import Data.List (partition)
import Info
import Prologue
import Source
import Language
import qualified Syntax as S
import Term

operators :: [Text]
operators = [ "and", "boolean_and", "or", "boolean_or", "bitwise_or", "bitwise_and", "shift", "relational", "comparison" ]

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
  | name == "unless_modifier" = case children of
    [ lhs, rhs ] -> do
      condition <- withRecord (setCategory (extract rhs) Negate) (S.Negate rhs)
      withDefaultInfo $ S.If condition [lhs]
    _ -> withDefaultInfo $ S.Error children
  | name == "unless_statement" = case children of
    ( expr : rest ) -> do
      condition <- withRecord (setCategory (extract expr) Negate) (S.Negate expr)
      withDefaultInfo $ S.If condition rest
    _ -> withDefaultInfo $ S.Error children
  | name == "until_modifier" = case children of
    [ lhs, rhs ] -> do
      condition <- withRecord (setCategory (extract rhs) Negate) (S.Negate rhs)
      withDefaultInfo $ S.While condition [lhs]
    _ -> withDefaultInfo $ S.Error children
  | name == "until_statement" = case children of
    ( expr : rest ) -> do
      condition <- withRecord (setCategory (extract expr) Negate) (S.Negate expr)
      withDefaultInfo $ S.While condition rest
    _ -> withDefaultInfo $ S.Error children
  | otherwise = withDefaultInfo $ case (name, children) of
    ("argument_pair", [ k, v ] ) -> S.Pair k v
    ("argument_pair", _ ) -> S.Error children
    ("keyword_parameter", [ k, v ] ) -> S.Pair k v
    -- NB: ("keyword_parameter", k) is a required keyword parameter, e.g.:
    --    def foo(name:); end
    -- Let it fall through to generate an Indexed syntax.
    ("optional_parameter", [ k, v ] ) -> S.Pair k v
    ("optional_parameter", _ ) -> S.Error children
    ("array", _ ) -> S.Array children
    ("assignment", [ identifier, value ]) -> S.Assignment identifier value
    ("assignment", _ ) -> S.Error children
    ("begin_statement", _ ) -> case partition (\x -> category (extract x) == Rescue) children of
      (rescues, rest) -> case partition (\x -> category (extract x) == Ensure || category (extract x) == Else) rest of
        (ensureElse, body) -> case ensureElse of
          [ elseBlock, ensure ]
            | Else <- category (extract elseBlock)
            , Ensure <- category (extract ensure) -> S.Try body rescues (Just elseBlock) (Just ensure)
          [ ensure, elseBlock ]
            | Ensure <- category (extract ensure)
            , Else <- category (extract elseBlock) -> S.Try body rescues (Just elseBlock) (Just ensure)
          [ elseBlock ] | Else <- category (extract elseBlock) -> S.Try body rescues (Just elseBlock) Nothing
          [ ensure ] | Ensure <- category (extract ensure) -> S.Try body rescues Nothing (Just ensure)
          _ -> S.Try body rescues Nothing Nothing
    ("case_statement", expr : body ) -> S.Switch expr body
    ("case_statement", _ ) -> S.Error children
    ("when_block", condition : body ) -> S.Case condition body
    ("when_block", _ ) -> S.Error children
    ("class_declaration", [ identifier, superclass, definitions ]) -> S.Class identifier (Just superclass) (toList (unwrap definitions))
    ("class_declaration", [ identifier, definitions ]) -> S.Class identifier Nothing (toList (unwrap definitions))
    ("class_declaration", _ ) -> S.Error children
    ("comment", _ ) -> S.Comment . toText $ slice range source
    ("conditional_assignment", [ identifier, value ]) -> S.ConditionalAssignment identifier value
    ("conditional_assignment", _ ) -> S.Error children
    ("conditional", condition : cases) -> S.Ternary condition cases
    ("conditional", _ ) -> S.Error children
    ("function_call", _ ) -> case children of
      member : args | MemberAccess <- category (extract member) -> case toList (unwrap member) of
        [target, method] -> S.MethodCall target method (toList . unwrap =<< args)
        _ -> S.Error children
      function : args -> S.FunctionCall function (toList . unwrap =<< args)
      _ -> S.Error children
    ("hash", _ ) -> S.Object $ foldMap toTuple children
    ("if_modifier", [ lhs, condition ]) -> S.If condition [lhs]
    ("if_modifier", _ ) -> S.Error children
    ("if_statement", condition : body ) -> S.If condition body
    ("if_statement", _ ) -> S.Error children
    ("elsif_block", condition : body ) -> S.If condition body
    ("elsif_block", _ ) -> S.Error children
    ("element_reference", [ base, element ]) -> S.SubscriptAccess base element
    ("element_reference", _ ) -> S.Error children
    ("for_statement", lhs : expr : rest ) -> S.For [lhs, expr] rest
    ("for_statement", _ ) -> S.Error children
    ("math_assignment", [ identifier, value ]) -> S.MathAssignment identifier value
    ("math_assignment", _ ) -> S.Error children
    ("member_access", [ base, property ]) -> S.MemberAccess base property
    ("member_access", _ ) -> S.Error children
    ("method_declaration", _ ) -> case children of
      identifier : params : body | Params <- category (extract params) -> S.Method identifier (toList (unwrap params)) body
      identifier : body -> S.Method identifier [] body
      _ -> S.Error children
    ("module_declaration", identifier : body ) -> S.Module identifier body
    ("module_declaration", _ ) -> S.Error children
    ("rescue_block", _ ) -> case children of
      args : lastException : rest
        | RescueArgs <- category (extract args)
        , RescuedException <- category (extract lastException) -> S.Rescue (toList (unwrap args) <> [lastException]) rest
      lastException : rest | RescuedException <- category (extract lastException) -> S.Rescue [lastException] rest
      args : body | RescueArgs <- category (extract args) -> S.Rescue (toList (unwrap args)) body
      body -> S.Rescue [] body
    ("rescue_modifier", [lhs, rhs] ) -> S.Rescue [lhs] [rhs]
    ("rescue_modifier", _ ) -> S.Error children
    ("return_statement", _ ) -> S.Return (listToMaybe children)
    ("while_modifier", [ lhs, condition ]) -> S.While condition [lhs]
    ("while_modifier", _ ) -> S.Error children
    ("while_statement", expr : rest ) -> S.While expr rest
    ("while_statement", _ ) -> S.Error children
    ("yield", _ ) -> S.Yield (listToMaybe children)
    _ | name `elem` operators -> S.Operator children
    _ | name `elem` functions -> case children of
          [ body ] -> S.AnonymousFunction [] [body]
          ( params : body ) -> S.AnonymousFunction (toList (unwrap params)) body
          _ -> S.Error children
    (_, []) -> S.Leaf . toText $ slice range source
    _  -> S.Indexed children
  where
    withRecord record syntax = pure $! cofree (record :< syntax)
    withCategory category syntax = do
      sourceSpan' <- sourceSpan
      pure $! cofree ((range .: category .: sourceSpan' .: RNil) :< syntax)
    withDefaultInfo syntax = case syntax of
      S.MethodCall{} -> withCategory MethodCall syntax
      _ -> withCategory (categoryForRubyName name) syntax

categoryForRubyName :: Text -> Category
categoryForRubyName = \case
  "and" -> BooleanOperator
  "argument_list" -> Args
  "argument_pair" -> ArgumentPair
  "array" -> ArrayLiteral
  "assignment" -> Assignment
  "begin_statement" -> Begin
  "bitwise_and" -> BitwiseOperator -- bitwise and, e.g &.
  "bitwise_or" -> BitwiseOperator -- bitwise or, e.g. ^, |.
  "block_parameter" -> BlockParameter
  "boolean_and" -> BooleanOperator -- boolean and, e.g. &&.
  "boolean_or" -> BooleanOperator -- boolean or, e.g. &&.
  "boolean" -> Boolean
  "case_statement" -> Case
  "class_declaration"  -> Class
  "comment" -> Comment
  "comparison" -> RelationalOperator -- comparison operator, e.g. <, <=, >=, >.
  "conditional_assignment" -> ConditionalAssignment
  "conditional" -> Ternary
  "element_reference" -> SubscriptAccess
  "else_block" -> Else
  "elsif_block" -> Elsif
  "ensure_block" -> Ensure
  "ERROR" -> Error
  "float" -> FloatLiteral
  "for_statement" -> For
  "formal_parameters" -> Params
  "function_call" -> FunctionCall
  "function" -> Function
  "hash_splat_parameter" -> HashSplatParameter
  "hash" -> Object
  "identifier" -> Identifier
  "if_modifier" -> If
  "if_statement" -> If
  "integer" -> IntegerLiteral
  "interpolation" -> Interpolation
  "keyword_parameter" -> KeywordParameter
  "math_assignment" -> MathAssignment
  "member_access" -> MemberAccess
  "method_declaration" -> Method
  "module_declaration"  -> Module
  "nil" -> Identifier
  "optional_parameter" -> OptionalParameter
  "or" -> BooleanOperator
  "program" -> Program
  "regex" -> Regex
  "relational" -> RelationalOperator -- relational operator, e.g. ==, !=, ===, <=>, =~, !~.
  "rescue_arguments" -> RescueArgs
  "rescue_block" -> Rescue
  "rescue_modifier" -> RescueModifier
  "rescued_exception" -> RescuedException
  "return_statement" -> Return
  "shift" -> BitwiseOperator -- bitwise shift, e.g <<, >>.
  "splat_parameter" -> SplatParameter
  "string" -> StringLiteral
  "subshell" -> Subshell
  "symbol" -> SymbolLiteral
  "unless_modifier" -> Unless
  "unless_statement" -> Unless
  "until_modifier" -> Until
  "until_statement" -> Until
  "when_block" -> When
  "while_modifier" -> While
  "while_statement" -> While
  "yield" -> Yield
  s -> Other s

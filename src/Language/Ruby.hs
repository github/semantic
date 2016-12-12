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
operators = ["binary", "unary", "range", "scope_resolution"]

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> IO [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ All child nodes (included unnamed productions) of the term as 'IO'. Only use this if you need it.
  -> IO (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children allChildren
  | name == "ERROR" = withDefaultInfo (S.Error children)
  | name == "unless_modifier" = case children of
    [ lhs, rhs ] -> do
      condition <- withRecord (setCategory (extract rhs) Negate) (S.Negate rhs)
      withDefaultInfo $ S.If condition [lhs]
    _ -> withDefaultInfo $ S.Error children
  | name == "unless" = case children of
    ( expr : rest ) -> do
      condition <- withRecord (setCategory (extract expr) Negate) (S.Negate expr)
      withDefaultInfo $ S.If condition rest
    _ -> withDefaultInfo $ S.Error children
  | name == "until_modifier" = case children of
    [ lhs, rhs ] -> do
      condition <- withRecord (setCategory (extract rhs) Negate) (S.Negate rhs)
      withDefaultInfo $ S.While condition [lhs]
    _ -> withDefaultInfo $ S.Error children
  | name == "until" = case children of
    ( expr : rest ) -> do
      condition <- withRecord (setCategory (extract expr) Negate) (S.Negate expr)
      withDefaultInfo $ S.While condition rest
    _ -> withDefaultInfo $ S.Error children
  | name `elem` operators = do
    allChildren' <- allChildren
    withDefaultInfo $ S.Operator allChildren'
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
    ("begin", _ ) -> case partition (\x -> category (extract x) == Rescue) children of
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
    ("case", expr : body ) -> S.Switch expr body
    ("case", _ ) -> S.Error children
    ("when", condition : body ) -> S.Case condition body
    ("when", _ ) -> S.Error children
    ("class", constant : rest ) -> case rest of
      ( superclass : body ) | Superclass <- category (extract superclass) -> S.Class constant (Just superclass) body
      _ -> S.Class constant Nothing rest
    ("class", _ ) -> S.Error children
    ("singleton_class", identifier : rest ) -> S.Class identifier Nothing rest
    ("singleton_class", _ ) -> S.Error children
    ("comment", _ ) -> S.Comment . toText $ slice range source
    ("conditional", condition : cases) -> S.Ternary condition cases
    ("conditional", _ ) -> S.Error children
    ("constant", _ ) -> S.Fixed children
    ("method_call", _ ) -> case children of
      member : args | MemberAccess <- category (extract member) -> case toList (unwrap member) of
        [target, method] -> S.MethodCall target method (toList . unwrap =<< args)
        _ -> S.Error children
      function : args -> S.FunctionCall function (toList . unwrap =<< args)
      _ -> S.Error children
    ("lambda", _) -> case children of
      [ body ] -> S.AnonymousFunction [] [body]
      ( params : body ) -> S.AnonymousFunction (toList (unwrap params)) body
      _ -> S.Error children
    ("hash", _ ) -> S.Object $ foldMap toTuple children
    ("if_modifier", [ lhs, condition ]) -> S.If condition [lhs]
    ("if_modifier", _ ) -> S.Error children
    ("if", condition : body ) -> S.If condition body
    ("if", _ ) -> S.Error children
    ("elsif", condition : body ) -> S.If condition body
    ("elsif", _ ) -> S.Error children
    ("element_reference", [ base, element ]) -> S.SubscriptAccess base element
    ("element_reference", _ ) -> S.Error children
    ("for", lhs : expr : rest ) -> S.For [lhs, expr] rest
    ("for", _ ) -> S.Error children
    ("operator_assignment", [ identifier, value ]) -> S.OperatorAssignment identifier value
    ("operator_assignment", _ ) -> S.Error children
    ("call", [ base, property ]) -> S.MemberAccess base property
    ("call", _ ) -> S.Error children
    ("method", _ ) -> case children of
      identifier : params : body | Params <- category (extract params) -> S.Method identifier (toList (unwrap params)) body
      identifier : body -> S.Method identifier [] body
      _ -> S.Error children
    ("module", constant : body ) -> S.Module constant body
    ("module", _ ) -> S.Error children
    ("rescue", _ ) -> case children of
      exceptions : exceptionVar : rest
        | RescueArgs <- category (extract exceptions)
        , RescuedException <- category (extract exceptionVar) -> S.Rescue (toList (unwrap exceptions) <> [exceptionVar]) rest
      exceptionVar : rest | RescuedException <- category (extract exceptionVar) -> S.Rescue [exceptionVar] rest
      exceptions : body | RescueArgs <- category (extract exceptions) -> S.Rescue (toList (unwrap exceptions)) body
      body -> S.Rescue [] body
    ("rescue_modifier", [lhs, rhs] ) -> S.Rescue [lhs] [rhs]
    ("rescue_modifier", _ ) -> S.Error children
    ("return", _ ) -> S.Return children
    ("while_modifier", [ lhs, condition ]) -> S.While condition [lhs]
    ("while_modifier", _ ) -> S.Error children
    ("while", expr : rest ) -> S.While expr rest
    ("while", _ ) -> S.Error children
    ("yield", _ ) -> S.Yield children
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
  "argument_list" -> Args
  "argument_pair" -> ArgumentPair
  "array" -> ArrayLiteral
  "assignment" -> Assignment
  "begin" -> Begin
  "binary" -> Binary
  "block_parameter" -> BlockParameter
  "boolean" -> Boolean
  "call" -> MemberAccess
  "case" -> Case
  "class"  -> Class
  "comment" -> Comment
  "conditional" -> Ternary
  "constant" -> Constant
  "element_reference" -> SubscriptAccess
  "else" -> Else
  "elsif" -> Elsif
  "ensure" -> Ensure
  "ERROR" -> Error
  "exception_variable" -> RescuedException
  "exceptions" -> RescueArgs
  "false" -> Boolean
  "float" -> NumberLiteral
  "for" -> For
  "formal_parameters" -> Params
  "hash_splat_parameter" -> HashSplatParameter
  "hash" -> Object
  "identifier" -> Identifier
  "if_modifier" -> If
  "if" -> If
  "instance_variable" -> Identifier
  "integer" -> IntegerLiteral
  "interpolation" -> Interpolation
  "keyword_parameter" -> KeywordParameter
  "method_call" -> MethodCall
  "method" -> Method
  "module"  -> Module
  "nil" -> Identifier
  "operator_assignment" -> OperatorAssignment
  "optional_parameter" -> OptionalParameter
  "pair" -> Pair
  "program" -> Program
  "range" -> RangeExpression
  "regex" -> Regex
  "rescue_modifier" -> RescueModifier
  "rescue" -> Rescue
  "return" -> Return
  "scope_resolution" -> ScopeOperator
  "self" -> Identifier
  "singleton_class"  -> SingletonClass
  "splat_parameter" -> SplatParameter
  "string" -> StringLiteral
  "subshell" -> Subshell
  "superclass" -> Superclass
  "symbol" -> SymbolLiteral
  "true" -> Boolean
  "unary" -> Unary
  "unless_modifier" -> Unless
  "unless" -> Unless
  "until_modifier" -> Until
  "until" -> Until
  "when" -> When
  "while_modifier" -> While
  "while" -> While
  "yield" -> Yield
  s -> Other s

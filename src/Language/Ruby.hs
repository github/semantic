{-# LANGUAGE DataKinds #-}
module Language.Ruby where

import Data.List (partition)
import Info
import Prologue
import Source hiding (null)
import Language
import qualified Syntax as S
import Term

termAssignment
  :: Source -- ^ The source of the term.
  -> Category -- ^ The category for the term.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> Maybe (S.Syntax Text (SyntaxTerm Text '[Range, Category, SourceSpan])) -- ^ The resulting term, in Maybe.
termAssignment _ category children
  = case (category, children) of
    (ArgumentPair, [ k, v ] ) -> Just $ S.Pair k v
    (KeywordParameter, [ k, v ] ) -> Just $ S.Pair k v
    -- NB: ("keyword_parameter", k) is a required keyword parameter, e.g.:
    --    def foo(name:); end
    -- Let it fall through to generate an Indexed syntax.
    (OptionalParameter, [ k, v ] ) -> Just $ S.Pair k v
    (AnonymousFunction, first : rest)
      | null rest -> Just $ S.AnonymousFunction [] [first]
      | otherwise -> Just $ S.AnonymousFunction (toList (unwrap first)) rest
    (ArrayLiteral, _ ) -> Just $ S.Array Nothing children
    (Assignment, [ identifier, value ]) -> Just $ S.Assignment identifier value
    (Begin, _ ) -> Just $ case partition (\x -> Info.category (extract x) == Rescue) children of
      (rescues, rest) -> case partition (\x -> Info.category (extract x) == Ensure || Info.category (extract x) == Else) rest of
        (ensureElse, body) -> case ensureElse of
          [ elseBlock, ensure ]
            | Else <- Info.category (extract elseBlock)
            , Ensure <- Info.category (extract ensure) -> S.Try body rescues (Just elseBlock) (Just ensure)
          [ ensure, elseBlock ]
            | Ensure <- Info.category (extract ensure)
            , Else <- Info.category (extract elseBlock) -> S.Try body rescues (Just elseBlock) (Just ensure)
          [ elseBlock ] | Else <- Info.category (extract elseBlock) -> S.Try body rescues (Just elseBlock) Nothing
          [ ensure ] | Ensure <- Info.category (extract ensure) -> S.Try body rescues Nothing (Just ensure)
          _ -> S.Try body rescues Nothing Nothing
    (Class, constant : superclass : body)
      | Superclass <- Info.category (extract superclass)
      -> Just $ S.Class constant [superclass] body
    (Class, constant : rest) -> Just $ S.Class constant [] rest
    (SingletonClass, identifier : rest) -> Just $ S.Class identifier [] rest
    (Case, _) -> Just $ uncurry S.Switch (Prologue.break ((== When) . Info.category . extract) children)
    (When, expr : body) -> Just $ S.Case expr body
    (Ternary, condition : cases) -> Just $ S.Ternary condition cases
    (MethodCall, fn : args)
      | MemberAccess <- Info.category (extract fn)
      , [target, method] <- toList (unwrap fn)
      -> Just $ S.MethodCall target method [] (toList . unwrap =<< args)
      | otherwise
      -> Just $ S.FunctionCall fn [] (toList . unwrap =<< args)
    (Object, _ ) -> Just . S.Object Nothing $ foldMap toTuple children
    (Modifier If, [ lhs, condition ]) -> Just $ S.If condition [lhs]
    (Modifier Unless, [lhs, rhs]) -> Just $ S.If (withRecord (setCategory (extract rhs) Negate) (S.Negate rhs)) [lhs]
    (Unless, expr : rest) -> Just $ S.If (withRecord (setCategory (extract expr) Negate) (S.Negate expr)) rest
    (Modifier Until, [ lhs, rhs ]) -> Just $ S.While (withRecord (setCategory (extract rhs) Negate) (S.Negate rhs)) [lhs]
    (Until, expr : rest) -> Just $ S.While (withRecord (setCategory (extract expr) Negate) (S.Negate expr)) rest
    (Elsif, condition : body ) -> Just $ S.If condition body
    (SubscriptAccess, [ base, element ]) -> Just $ S.SubscriptAccess base element
    (For, lhs : expr : rest ) -> Just $ S.For [lhs, expr] rest
    (OperatorAssignment, [ identifier, value ]) -> Just $ S.OperatorAssignment identifier value
    (MemberAccess, [ base, property ]) -> Just $ S.MemberAccess base property
    (SingletonMethod, expr : methodName : rest)
      | params : body <- rest
      , Params <- Info.category (extract params)
      -> Just $ S.Method [] methodName (Just expr) [params] body
      | Identifier <- Info.category (extract methodName)
      -> Just $ S.Method [] methodName (Just expr) [] rest
    (Method, identifier : rest)
      | params : body <- rest
      , Params <- Info.category (extract params)
      -> Just $ S.Method [] identifier Nothing [params] body
      | otherwise
      -> Just $ S.Method [] identifier Nothing [] rest
    (Module, constant : body ) -> Just $ S.Module constant body
    (Modifier Rescue, [lhs, rhs] ) -> Just $ S.Rescue [lhs] [rhs]
    (Rescue, exceptions : exceptionVar : rest)
      | RescueArgs <- Info.category (extract exceptions)
      , RescuedException <- Info.category (extract exceptionVar)
      -> Just $ S.Rescue (toList (unwrap exceptions) <> [exceptionVar]) rest
    (Rescue, exceptionVar : rest)
      | RescuedException <- Info.category (extract exceptionVar)
      -> Just $ S.Rescue [exceptionVar] rest
    (Rescue, exceptions : body)
      | RescueArgs <- Info.category (extract exceptions)
      -> Just $ S.Rescue (toList (unwrap exceptions)) body
    (Rescue, body) -> Just $ S.Rescue [] body
    (Modifier While, [ lhs, condition ]) -> Just $ S.While condition [lhs]
    _ | category `elem` [ BeginBlock, EndBlock ] -> Just $ S.BlockStatement children
    _  -> Nothing
  where
    withRecord record syntax = cofree (record :< syntax)

categoryForRubyName :: Text -> Category
categoryForRubyName name = case name of
  "argument_list_with_parens" -> Args
  "argument_list" -> Args
  "argument_pair" -> ArgumentPair
  "array" -> ArrayLiteral
  "assignment" -> Assignment
  "begin_block" -> BeginBlock
  "begin" -> Begin
  "binary" -> Binary
  "block_parameter" -> BlockParameter
  "block_parameters" -> Params
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
  "empty_statement" -> Empty
  "end_block" -> EndBlock
  "ensure" -> Ensure
  "exception_variable" -> RescuedException
  "exceptions" -> RescueArgs
  "false" -> Boolean
  "float" -> NumberLiteral
  "for" -> For
  "hash_splat_parameter" -> HashSplatParameter
  "hash" -> Object
  "identifier" -> Identifier
  "if_modifier" -> Modifier If
  "if" -> If
  "instance_variable" -> Identifier
  "integer" -> IntegerLiteral
  "interpolation" -> Interpolation
  "keyword_parameter" -> KeywordParameter
  "lambda_parameters" -> Params
  "lambda" -> AnonymousFunction
  "left_assignment_list" -> Args
  "method_call" -> MethodCall
  "method_parameters" -> Params
  "method" -> Method
  "module"  -> Module
  "nil" -> Identifier
  "operator_assignment" -> OperatorAssignment
  "optional_parameter" -> OptionalParameter
  "pair" -> Pair
  "pattern" -> Args
  "program" -> Program
  "range" -> RangeExpression
  "regex" -> Regex
  "rescue_modifier" -> Modifier Rescue
  "rescue" -> Rescue
  "rest_assignment" -> SplatParameter
  "return" -> Return
  "scope_resolution" -> ScopeOperator
  "self" -> Identifier
  "singleton_class"  -> SingletonClass
  "singleton_method" -> SingletonMethod
  "splat_parameter" -> SplatParameter
  "string" -> StringLiteral
  "subshell" -> Subshell
  "superclass" -> Superclass
  "symbol" -> SymbolLiteral
  "true" -> Boolean
  "unary" -> Unary
  "unless_modifier" -> Modifier Unless
  "unless" -> Unless
  "until_modifier" -> Modifier Until
  "until" -> Until
  "when" -> When
  "while_modifier" -> Modifier While
  "while" -> While
  "yield" -> Yield
  s -> Other s

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

operators :: [Category]
operators = [ Binary, Unary, RangeExpression, ScopeOperator ]

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> SourceSpan -- ^ The span that the term occupies.
  -> Category -- ^ The node’s Category.
  -> Range -- ^ The character range that the term occupies.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> IO [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ All child nodes (included unnamed productions) of the term as 'IO'. Only use this if you need it.
  -> IO (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termConstructor source sourceSpan category range children allChildren
  | category == Error = pure $! withDefaultInfo (S.Error children)
  | category `elem` operators = do
    allChildren' <- allChildren
    pure $! withDefaultInfo $ S.Operator allChildren'
  | otherwise = pure . withDefaultInfo $ case (category, children) of
    (ArgumentPair, [ k, v ] ) -> S.Pair k v
    (ArgumentPair, _ ) -> S.Error children
    (KeywordParameter, [ k, v ] ) -> S.Pair k v
    -- NB: ("keyword_parameter", k) is a required keyword parameter, e.g.:
    --    def foo(name:); end
    -- Let it fall through to generate an Indexed syntax.
    (OptionalParameter, [ k, v ] ) -> S.Pair k v
    (OptionalParameter, _ ) -> S.Error children
    (ArrayLiteral, _ ) -> S.Array Nothing children
    (Assignment, [ identifier, value ]) -> S.Assignment identifier value
    (Assignment, _ ) -> S.Error children
    (Begin, _ ) -> case partition (\x -> Info.category (extract x) == Rescue) children of
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
    (Case, expr : body ) -> S.Switch (Just expr) body
    (Case, _ ) -> S.Error children
    (When, condition : body ) -> S.Case condition body
    (When, _ ) -> S.Error children
    (Class, constant : rest ) -> case rest of
      ( superclass : body ) | Superclass <- Info.category (extract superclass) -> S.Class constant (Just superclass) body
      _ -> S.Class constant Nothing rest
    (Class, _ ) -> S.Error children
    (SingletonClass, identifier : rest ) -> S.Class identifier Nothing rest
    (SingletonClass, _ ) -> S.Error children
    (Comment, _ ) -> S.Comment . toText $ slice range source
    (Ternary, condition : cases) -> S.Ternary condition cases
    (Ternary, _ ) -> S.Error children
    (Constant, _ ) -> S.Fixed children
    (MethodCall, _ ) -> case children of
      member : args | MemberAccess <- Info.category (extract member) -> case toList (unwrap member) of
        [target, method] -> S.MethodCall target method (toList . unwrap =<< args)
        _ -> S.Error children
      function : args -> S.FunctionCall function (toList . unwrap =<< args)
      _ -> S.Error children
    (Other "lambda", _) -> case children of
      [ body ] -> S.AnonymousFunction [] [body]
      ( params : body ) -> S.AnonymousFunction (toList (unwrap params)) body
      _ -> S.Error children
    (Object, _ ) -> S.Object Nothing $ foldMap toTuple children
    (Modifier If, [ lhs, condition ]) -> S.If condition [lhs]
    (Modifier If, _) -> S.Error children
    (If, condition : body ) -> S.If condition body
    (If, _ ) -> S.Error children
    (Modifier Unless, [lhs, rhs]) -> S.If (withRecord (setCategory (extract rhs) Negate) (S.Negate rhs)) [lhs]
    (Modifier Unless, _) -> S.Error children
    (Unless, expr : rest) -> S.If (withRecord (setCategory (extract expr) Negate) (S.Negate expr)) rest
    (Unless, _) -> S.Error children
    (Modifier Until, [ lhs, rhs ]) -> S.While (withRecord (setCategory (extract rhs) Negate) (S.Negate rhs)) [lhs]
    (Modifier Until, _) -> S.Error children
    (Until, expr : rest) -> S.While (withRecord (setCategory (extract expr) Negate) (S.Negate expr)) rest
    (Until, _) -> S.Error children
    (Elsif, condition : body ) -> S.If condition body
    (Elsif, _ ) -> S.Error children
    (SubscriptAccess, [ base, element ]) -> S.SubscriptAccess base element
    (SubscriptAccess, _ ) -> S.Error children
    (For, lhs : expr : rest ) -> S.For [lhs, expr] rest
    (For, _ ) -> S.Error children
    (OperatorAssignment, [ identifier, value ]) -> S.OperatorAssignment identifier value
    (OperatorAssignment, _ ) -> S.Error children
    (MemberAccess, [ base, property ]) -> S.MemberAccess base property
    (MemberAccess, _ ) -> S.Error children
    (Method, _ ) -> case children of
      identifier : params : body | Params <- Info.category (extract params) -> S.Method identifier Nothing (toList (unwrap params)) body
      identifier : body -> S.Method identifier Nothing [] body
      _ -> S.Error children
    (Module, constant : body ) -> S.Module constant body
    (Module, _ ) -> S.Error children
    (Modifier Rescue, [lhs, rhs] ) -> S.Rescue [lhs] [rhs]
    (Modifier Rescue, _) -> S.Error children
    (Rescue, _ ) -> case children of
      exceptions : exceptionVar : rest
        | RescueArgs <- Info.category (extract exceptions)
        , RescuedException <- Info.category (extract exceptionVar) -> S.Rescue (toList (unwrap exceptions) <> [exceptionVar]) rest
      exceptionVar : rest | RescuedException <- Info.category (extract exceptionVar) -> S.Rescue [exceptionVar] rest
      exceptions : body | RescueArgs <- Info.category (extract exceptions) -> S.Rescue (toList (unwrap exceptions)) body
      body -> S.Rescue [] body
    (Return, _ ) -> S.Return children
    (Modifier While, [ lhs, condition ]) -> S.While condition [lhs]
    (Modifier While, _) -> S.Error children
    (While, expr : rest ) -> S.While expr rest
    (While, _ ) -> S.Error children
    (Yield, _ ) -> S.Yield children
    _ | category `elem` [ BeginBlock, EndBlock ] -> S.BlockStatement children
    (_, []) -> S.Leaf . toText $ slice range source
    _  -> S.Indexed children
  where
    withRecord record syntax = cofree (record :< syntax)
    withCategory category syntax =
      cofree ((range :. category :. sourceSpan :. Nil) :< syntax)
    withDefaultInfo syntax = case syntax of
      S.MethodCall{} -> withCategory MethodCall syntax
      _ -> withCategory category syntax

categoryForRubyName :: Text -> Category
categoryForRubyName = \case
  "argument_list" -> Args
  "argument_pair" -> ArgumentPair
  "array" -> ArrayLiteral
  "assignment" -> Assignment
  "begin_block" -> BeginBlock
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
  "end_block" -> EndBlock
  "ensure" -> Ensure
  "exception_variable" -> RescuedException
  "exceptions" -> RescueArgs
  "false" -> Boolean
  "float" -> NumberLiteral
  "for" -> For
  "formal_parameters" -> Params
  "hash_splat_parameter" -> HashSplatParameter
  "hash" -> Object
  "identifier" -> Identifier
  "if_modifier" -> Modifier If
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
  "rescue_modifier" -> Modifier Rescue
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
  "unless_modifier" -> Modifier Unless
  "unless" -> Unless
  "until_modifier" -> Modifier Until
  "until" -> Until
  "when" -> When
  "while_modifier" -> Modifier While
  "while" -> While
  "yield" -> Yield
  s -> Other s

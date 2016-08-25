{-# LANGUAGE DataKinds #-}
module Parser where

import Prologue hiding (Constructor)
import Data.Record
import Data.Text (pack)
import Category as C
import Info
import qualified Syntax as S
import Term
import qualified Data.Set as Set
import Source hiding (uncons)
import SourceSpan

-- | A function that takes a source blob and returns an annotated AST.
-- | The return is in the IO monad because some of the parsers are written in C
-- | and aren't pure.
type Parser f a = SourceBlob -> IO (Cofree f a)

-- | Whether a category is an Operator Category
isOperator :: Category -> Bool
isOperator = flip Set.member (Set.fromList [ Operator, BinaryOperator, BitwiseOperator, RelationalOperator ])

-- | Construct a term given source, the span covered, the annotation for the term, and its children.
--
-- This is typically called during parsing, building terms up leaf-to-root.
termConstructor :: (Show (Record fields), HasField fields Category, HasField fields Range)
  => Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Record fields -- ^ The annotation for the term.
  -> [Term Text (Record fields)] -- ^ The child nodes of the term.
  -> IO (Term Text (Record fields)) -- ^ The resulting term, in IO.
termConstructor source sourceSpan info children = case category info of
  Return -> withDefaultInfo $ S.Return (listToMaybe children)
  Assignment -> case children of
    [ identifier, value ] -> withDefaultInfo $ S.Assignment identifier value
    _ -> errorWith children
  MathAssignment -> case children of
    [ identifier, value ] -> withDefaultInfo $ S.MathAssignment identifier value
    _ -> errorWith children
  MemberAccess -> case children of
    [ base, property ] -> withDefaultInfo $ S.MemberAccess base property
    _ -> errorWith children
  SubscriptAccess -> case children of
    [ base, element ] -> withDefaultInfo $ S.SubscriptAccess base element
    _ -> errorWith children
  op | isOperator op -> withDefaultInfo $ S.Operator children
  CommaOperator -> withDefaultInfo $ case children of
    [ child, rest ] -> S.Indexed $ child : toList (unwrap rest)
    _ -> S.Indexed children
  Function -> case children of
    [ body ] -> withDefaultInfo $ S.AnonymousFunction Nothing body
    [ params, body ] | (info :< _) <- runCofree params, Params == category info ->
      withDefaultInfo $ S.AnonymousFunction (Just params) body
    [ id, body ] | (info :< _) <- runCofree id, Identifier == category info ->
      withDefaultInfo $ S.Function id Nothing body
    [ id, params, body ] | (info :< _) <- runCofree id, Identifier == category info ->
      withDefaultInfo $ S.Function id (Just params) body
    _ -> errorWith children
  FunctionCall -> case runCofree <$> children of
    [ (_ :< S.MemberAccess{..}), (_ :< S.Args args) ] ->
      pure $! cofree $ setCategory info MethodCall :< S.MethodCall memberId property args
    [ (_ :< S.MemberAccess{..}) ] ->
      pure $! cofree $ setCategory info MethodCall :< S.MethodCall memberId property []
    (x:xs) ->
      withDefaultInfo $ S.FunctionCall (cofree x) (cofree <$> xs)
    _ -> errorWith children
  Ternary -> case children of
    (condition:cases) -> withDefaultInfo $ S.Ternary condition cases
    _ -> errorWith children
  Args -> withDefaultInfo $ S.Args children
  VarAssignment | [ x, y ] <- children -> withDefaultInfo $ S.VarAssignment x y
  VarDecl -> withDefaultInfo . S.Indexed $ toVarDecl <$> children
  Switch | (expr:rest) <- children -> withDefaultInfo $ S.Switch expr rest
  Case | [ expr, body ] <- children -> withDefaultInfo $ S.Case expr body
  Object -> withDefaultInfo . S.Object $ foldMap toTuple children
  Pair -> withDefaultInfo $ S.Fixed children
  C.Error -> errorWith children
  If | Just (expr, clauses) <- uncons children -> case clauses of
    [ clause1, clause2 ] -> withDefaultInfo $ S.If expr clause1 (Just clause2)
    [ clause ] -> withDefaultInfo $ S.If expr clause Nothing
    _ -> errorWith children

  For | Just (exprs, body) <- unsnoc children -> withDefaultInfo $ S.For exprs body
  While | [ expr, body ] <- children -> withDefaultInfo $ S.While expr body
  DoWhile | [ expr, body ] <- children -> withDefaultInfo $ S.DoWhile expr body
  Throw | [ expr ] <- children -> withDefaultInfo $ S.Throw expr
  Constructor | [ expr ] <- children -> withDefaultInfo $ S.Constructor expr
  Try -> case children of
    [ body ] -> withDefaultInfo $ S.Try body Nothing Nothing
    [ body, catch ] | Catch <- category (extract catch) -> withDefaultInfo $ S.Try body (Just catch) Nothing
    [ body, finally ] | Finally <- category (extract finally) -> withDefaultInfo $ S.Try body Nothing (Just finally)
    [ body, catch, finally ]
      | Catch <- category (extract catch),
        Finally <- category (extract finally) -> withDefaultInfo $ S.Try body (Just catch) (Just finally)
    _ -> errorWith children
  ArrayLiteral -> withDefaultInfo $ S.Array children

  Method -> case children of
    [ identifier, params, exprs ] |
      Params == category (extract params),
      S.Indexed params' <- unwrap params -> withDefaultInfo $ S.Method identifier params' (toList (unwrap exprs))
    [ identifier, exprs ] ->
      withDefaultInfo $ S.Method identifier mempty (toList (unwrap exprs))
    _ -> errorWith children

  Class -> case children of
    [ identifier, superclass, definitions ] ->
      withDefaultInfo $ S.Class identifier (Just superclass) (toList (unwrap definitions))
    [ identifier, definitions ] ->
      withDefaultInfo $ S.Class identifier Nothing (toList (unwrap definitions))
    _ -> errorWith children

  _ -> case children of
    [] -> withDefaultInfo . S.Leaf . pack . toString $ slice (characterRange info) source
    _ -> withDefaultInfo $ S.Indexed children
  where withDefaultInfo syntax = pure $! cofree (info :< syntax)
        errorWith children = do
          sourceSpan' <- sourceSpan
          withDefaultInfo (S.Error sourceSpan' children)

javascriptTermConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [Term Text (Record '[Range, Category])] -- ^ The child nodes of the term.
  -> IO (Term Text (Record '[Range, Category])) -- ^ The resulting term, in IO.
javascriptTermConstructor source sourceSpan name range children
  | name == "ERROR" = sourceSpan >>= withDefaultInfo . (`S.Error` children)
  | otherwise = withDefaultInfo $ case (name, children) of
  ("return_statement", _) -> S.Return (listToMaybe children)
  ("assignment", [ identifier, value ]) -> S.Assignment identifier value
  ("math_assignment", [ identifier, value ]) -> S.MathAssignment identifier value
  ("member_access", [ base, property ]) -> S.MemberAccess base property
  ("subscript_access", [ base, element ]) -> S.SubscriptAccess base element
  ("comma_op", [ child, rest ]) -> S.Indexed $ child : toList (unwrap rest)
  _ | name `elem` [ "arrow_function", "generator_function", "function" ] -> case children of
    [ body ] -> S.AnonymousFunction Nothing body
    [ params, body ] -> S.AnonymousFunction (Just params) body
    [ id, params, body ] -> S.Function id (Just params) body
    _ -> S.Indexed children
  ("function_call", _) -> case runCofree <$> children of
    [ (_ :< S.MemberAccess{..}), (_ :< S.Args args) ] -> S.MethodCall memberId property args
    [ (_ :< S.MemberAccess{..}) ] -> S.MethodCall memberId property []
    (x:xs) -> S.FunctionCall (cofree x) (cofree <$> xs)
    _ -> S.Indexed children
  ("ternary", (condition:cases)) -> S.Ternary condition cases
  ("arguments", _) -> S.Args children
  ("var_assignment", [ x, y ]) -> S.VarAssignment x y
  ("var_declaration", _) -> S.Indexed $ toVarDecl <$> children
  ("switch_statement", (expr:rest)) -> S.Switch expr rest
  ("case", [ expr, body ]) -> S.Case expr body
  ("object", _) -> S.Object $ foldMap toTuple children
  ("pair", _) -> S.Fixed children
  ("if_statement", [ expr, clause1, clause2 ]) -> S.If expr clause1 (Just clause2)
  ("if_statement", [ expr, clause ]) -> S.If expr clause Nothing
  ("for_in_statement", _) | Just (exprs, body) <- unsnoc children -> S.For exprs body
  ("for_of_statement", _) | Just (exprs, body) <- unsnoc children -> S.For exprs body
  ("while_statement", [ expr, body ]) -> S.While expr body
  ("do_statement", [ expr, body ]) -> S.DoWhile expr body
  ("throw_statement", [ expr ]) -> S.Throw expr
  ("new_expression", [ expr ]) -> S.Constructor expr
  ("try_statement", [ body ]) -> S.Try body Nothing Nothing
  ("try_statement", [ body, catch ]) | Catch <- category (extract catch) -> S.Try body (Just catch) Nothing
  ("try_statement", [ body, finally ]) | Finally <- category (extract finally) -> S.Try body Nothing (Just finally)
  ("try_statement", [ body, catch, finally ])
    | Catch <- category (extract catch)
    , Finally <- category (extract finally) -> S.Try body (Just catch) (Just finally)
  ("array", _) -> S.Array children
  ("method_definition", [ identifier, params, exprs ]) -> S.Method identifier (toList (unwrap params)) (toList (unwrap exprs))
  ("method_definition", [ identifier, exprs ]) -> S.Method identifier [] (toList (unwrap exprs))
  ("class", [ identifier, superclass, definitions ]) -> S.Class identifier (Just superclass) (toList (unwrap definitions))
  ("class", [ identifier, definitions ]) -> S.Class identifier Nothing (toList (unwrap definitions))

  (_, []) -> S.Leaf . toText $ slice range source
  _ -> S.Indexed children
  where withDefaultInfo syntax@(S.MethodCall _ _ _) = pure $! cofree ((range .:  MethodCall .: RNil) :< syntax)
        withDefaultInfo syntax = pure $! cofree ((range .: categoryForJavaScriptProductionName name .: RNil) :< syntax)

categoryForJavaScriptProductionName :: Text -> Category
categoryForJavaScriptProductionName name = case name of
  "object" -> Object
  "expression_statement" -> ExpressionStatements
  "this_expression" -> Identifier
  "null" -> Identifier
  "undefined" -> Identifier
  "arrow_function" -> Function
  "generator_function" -> Function
  "math_op" -> BinaryOperator -- bitwise operator, e.g. +, -, *, /.
  "bool_op" -> BinaryOperator -- boolean operator, e.g. ||, &&.
  "comma_op" -> CommaOperator -- comma operator, e.g. expr1, expr2.
  "delete_op" -> Operator -- delete operator, e.g. delete x[2].
  "type_op" -> Operator -- type operator, e.g. typeof Object.
  "void_op" -> Operator -- void operator, e.g. void 2.
  "for_in_statement" -> For
  "for_of_statement" -> For
  "new_expression" -> Constructor
  "class"  -> Class
  "catch" -> Catch
  "finally" -> Finally
  "if_statement" -> If
  "empty_statement" -> Empty
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
  "throw_statement" -> Throw
  "try_statement" -> Try
  "method_definition" -> Method
  "comment" -> Comment
  "bitwise_op" -> BitwiseOperator
  "rel_op" -> RelationalOperator
  _ -> Other name

toVarDecl :: (HasField fields Category) => Term Text (Record fields) -> Term Text (Record fields)
toVarDecl child = cofree $ (setCategory (extract child) VarDecl :< S.VarDecl child)

toTuple :: Term Text (Record fields) -> [Term Text (Record fields)]
toTuple child | S.Indexed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Fixed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Leaf c <- unwrap child = [cofree (extract child :< S.Comment c)]
toTuple child = pure child

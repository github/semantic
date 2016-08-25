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
  Switch | [ expr, body ] <- children -> withDefaultInfo $ S.Case expr body
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
javascriptTermConstructor source sourceSpan name range children = withDefaultInfo $ case name of
  "return_statement" -> S.Return (listToMaybe children)
  _ -> S.Indexed children
  where withDefaultInfo = pure . cofree . ((range .: Other name .: RNil) :<)


toVarDecl :: (HasField fields Category) => Term Text (Record fields) -> Term Text (Record fields)
toVarDecl child = cofree $ (setCategory (extract child) VarDecl :< S.VarDecl child)

toTuple :: Term Text (Record fields) -> [Term Text (Record fields)]
toTuple child | S.Indexed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Fixed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Leaf c <- unwrap child = [cofree (extract child :< S.Comment c)]
toTuple child = pure child

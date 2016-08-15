{-# LANGUAGE ScopedTypeVariables #-}
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
isOperator = flip Set.member (Set.fromList [ Operator, BinaryOperator ])

-- | Given a function that maps production names to sets of categories, produce
-- | a Constructor.
termConstructor :: forall fields. (Show (Record fields), HasField fields Category, HasField fields Range) => Source Char -> SourceSpan -> (Record fields) -> [Term Text (Record fields)] -> Term Text (Record fields)
termConstructor source sourceSpan info = cofree . construct
  where
    withDefaultInfo syntax = (info :< syntax)
    errorWith = (seq sourceSpan) . withDefaultInfo . S.Error sourceSpan
    construct :: (Show (Record fields), HasField fields Category, HasField fields Range) => [Term Text (Record fields)] -> CofreeF (S.Syntax Text) (Record fields) (Term Text (Record fields))
    construct [] = case category info of
      Return -> withDefaultInfo $ S.Return Nothing -- Map empty return statements to Return Nothing
      _ -> withDefaultInfo . S.Leaf . pack . toString $ slice (characterRange info) source
    construct children | Return == category info =
      withDefaultInfo $ S.Return (listToMaybe children)
    construct children | Assignment == category info = case children of
      (identifier:value:[]) -> withDefaultInfo $ S.Assignment identifier value
      children -> errorWith children
    construct children | MathAssignment == category info = case children of
      (identifier:value:[]) -> withDefaultInfo $ S.MathAssignment identifier value
      children -> errorWith children
    construct children | MemberAccess == category info = case children of
      (base:property:[]) -> withDefaultInfo $ S.MemberAccess base property
      children -> errorWith children
    construct children | SubscriptAccess == category info = case children of
      (base:element:[]) -> withDefaultInfo $ S.SubscriptAccess base element
      _ -> errorWith children
    construct children | isOperator (category info) = withDefaultInfo $ S.Operator children
    construct children | Function == category info = case children of
      (body:[]) -> withDefaultInfo $ S.Function Nothing Nothing body
      (params:body:[]) | (info :< _) <- runCofree params, Params == category info ->
        withDefaultInfo $ S.Function Nothing (Just params) body
      (id:body:[]) | (info :< _) <- runCofree id, Identifier == category info ->
        withDefaultInfo $ S.Function (Just id) Nothing body
      (id:params:body:[]) | (info :< _) <- runCofree id, Identifier == category info ->
        withDefaultInfo $ S.Function (Just id) (Just params) body
      _ -> errorWith children

    construct children | FunctionCall == category info = case runCofree <$> children of
      [ (_ :< S.MemberAccess{..}), params@(_ :< S.Args{}) ] ->
        setCategory info MethodCall :< S.MethodCall memberId property (cofree params)
      (x:xs) ->
        withDefaultInfo $ S.FunctionCall (cofree x) (cofree <$> xs)
      _ -> errorWith children

    construct children | Ternary == category info = case children of
      (condition:cases) -> withDefaultInfo $ S.Ternary condition cases
      _ -> errorWith children
    construct children | Args == category info = withDefaultInfo $ S.Args children
    construct children | VarAssignment == category info
                         , [x, y] <- children = withDefaultInfo $ S.VarAssignment x y
    construct children | VarDecl == category info = withDefaultInfo . S.Indexed $ toVarDecl <$> children
      where
        toVarDecl :: (HasField fields Category) => Term Text (Record fields) -> Term Text (Record fields)
        toVarDecl child = cofree $ (setCategory (extract child) VarDecl :< S.VarDecl child)

    construct children | Switch == category info, (expr:_) <- children =
      withDefaultInfo $ S.Switch expr children

    construct children | Case == category info, [expr, body] <- children =
      withDefaultInfo $ S.Case expr body

    construct children | Object == category info = withDefaultInfo . S.Object $ foldMap toTuple children
      where
        toTuple :: Term Text (Record fields) -> [Term Text (Record fields)]
        toTuple child | S.Indexed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
        toTuple child | S.Fixed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
        toTuple child | S.Leaf c <- unwrap child = [cofree (extract child :< S.Comment c)]
        toTuple child = pure child

    construct children | Pair == (category info) = withDefaultInfo $ S.Fixed children
    construct children | C.Error == category info =
      errorWith children
    construct children | If == category info, Just (expr, clauses) <- uncons children =
      case clauses of
        [clause1, clause2] -> withDefaultInfo $ S.If expr clause1 (Just clause2)
        [clause] -> withDefaultInfo $ S.If expr clause Nothing
        _ -> errorWith children
    construct children | For == category info, Just (exprs, body) <- unsnoc children =
      withDefaultInfo $ S.For exprs body
    construct children | While == category info, [expr, body] <- children =
      withDefaultInfo $ S.While expr body
    construct children | DoWhile == category info, [expr, body] <- children =
      withDefaultInfo $ S.DoWhile expr body
    construct children | Throw == category info, [expr] <- children =
      withDefaultInfo $ S.Throw expr
    construct children | Constructor == category info, [expr] <- children =
      withDefaultInfo $ S.Constructor expr
    construct children | Try == category info = case children of
      [body] -> withDefaultInfo $ S.Try body Nothing Nothing
      [body, catch] | Catch <- category (extract catch) -> withDefaultInfo $ S.Try body (Just catch) Nothing
      [body, finally] | Finally <- category (extract finally) -> withDefaultInfo $ S.Try body Nothing (Just finally)
      [body, catch, finally] | Catch <- category (extract catch),
                               Finally <- category (extract finally) ->
        withDefaultInfo $ S.Try body (Just catch) (Just finally)
      _ -> errorWith children
    construct children | ArrayLiteral == category info =
      withDefaultInfo $ S.Array children
    construct children | Method == category info = case children of
      [identifier, params, exprs] |
        Params == category (extract params),
        S.Indexed params' <- unwrap params ->
          withDefaultInfo $ S.Method identifier params' (toList (unwrap exprs))
      [identifier, exprs] ->
        withDefaultInfo $ S.Method identifier mempty (toList (unwrap exprs))
      _ -> errorWith children
    construct children | Class == category info = case children of
      [identifier, superclass, definitions] ->
        withDefaultInfo $ S.Class identifier (Just superclass) (toList (unwrap definitions))
      [identifier, definitions] ->
        withDefaultInfo $ S.Class identifier Nothing (toList (unwrap definitions))
      _ -> errorWith children
    construct children =
      withDefaultInfo $ S.Indexed children

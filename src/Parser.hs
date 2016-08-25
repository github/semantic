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
isOperator = flip Set.member (Set.fromList [ Operator, BinaryOperator, BitwiseOperator, RelationalOperator ])

-- | Construct a term given source, the span covered, the annotation for the term, and its children.
--
-- This is typically called during parsing, building terms up leaf-to-root.
termConstructor :: forall fields. (Show (Record fields), HasField fields Category, HasField fields Range)
  => Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Record fields -- ^ The annotation for the term.
  -> [Term Text (Record fields)] -- ^ The child nodes of the term.
  -> IO (Term Text (Record fields)) -- ^ The resulting term, in IO.
termConstructor source sourceSpan info = fmap (cofree . (info :<)) . construct
  where
    errorWith children = do
      sourceSpan' <- sourceSpan
      pure (S.Error sourceSpan' children)
    construct :: (Show (Record fields), HasField fields Category, HasField fields Range) => [Term Text (Record fields)] -> IO (S.Syntax Text (Term Text (Record fields)))
    construct [] = pure $ case category info of
      Return -> S.Return Nothing -- Map empty return statements to Return Nothing
      _ -> S.Leaf . pack . toString $ slice (characterRange info) source
    construct children | Return == category info =
      pure $ S.Return (listToMaybe children)
    construct children | Assignment == category info = case children of
      (identifier:value:[]) -> pure $ S.Assignment identifier value
      children -> errorWith children
    construct children | MathAssignment == category info = case children of
      (identifier:value:[]) -> pure $ S.MathAssignment identifier value
      children -> errorWith children
    construct children | MemberAccess == category info = case children of
      (base:property:[]) -> pure $ S.MemberAccess base property
      children -> errorWith children
    construct children | SubscriptAccess == category info = case children of
      (base:element:[]) -> pure $ S.SubscriptAccess base element
      _ -> errorWith children
    construct children | isOperator (category info) = pure $ S.Operator children
    construct children | CommaOperator == category info = pure $ case children of
      [child, rest] | S.Indexed cs <- unwrap rest -> S.Indexed $ child : toList cs
      _ -> S.Indexed children
    construct children | Function == category info = case children of
      (body:[]) -> pure $ S.AnonymousFunction Nothing body
      (params:body:[]) | (info :< _) <- runCofree params, Params == category info ->
        pure $ S.AnonymousFunction (Just params) body
      (id:body:[]) | (info :< _) <- runCofree id, Identifier == category info ->
        pure $ S.Function id Nothing body
      (id:params:body:[]) | (info :< _) <- runCofree id, Identifier == category info ->
        pure $ S.Function id (Just params) body
      _ -> errorWith children

    construct children | FunctionCall == category info = case runCofree <$> children of
      [ (_ :< S.MemberAccess{..}), (_ :< S.Args args) ] ->
        pure $ S.MethodCall memberId property args
      [ (_ :< S.MemberAccess{..}) ] ->
        pure $ S.MethodCall memberId property []
      (x:xs) ->
        pure $ S.FunctionCall (cofree x) (cofree <$> xs)
      _ -> errorWith children

    construct children | Ternary == category info = case children of
      (condition:cases) -> pure $ S.Ternary condition cases
      _ -> errorWith children
    construct children | Args == category info = pure $ S.Args children
    construct children | VarAssignment == category info
                         , [x, y] <- children = pure $ S.VarAssignment x y
    construct children | VarDecl == category info = pure . S.Indexed $ toVarDecl <$> children
      where
        toVarDecl :: (HasField fields Category) => Term Text (Record fields) -> Term Text (Record fields)
        toVarDecl child = cofree $ (setCategory (extract child) VarDecl :< S.VarDecl child)

    construct children | Switch == category info, (expr:_) <- children =
      pure $ S.Switch expr children

    construct children | Case == category info, [expr, body] <- children =
      pure $ S.Case expr body

    construct children | Object == category info = pure . S.Object $ foldMap toTuple children
      where
        toTuple :: Term Text (Record fields) -> [Term Text (Record fields)]
        toTuple child | S.Indexed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
        toTuple child | S.Fixed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
        toTuple child | S.Leaf c <- unwrap child = [cofree (extract child :< S.Comment c)]
        toTuple child = pure child

    construct children | Pair == (category info) = pure $ S.Fixed children
    construct children | C.Error == category info =
      errorWith children
    construct children | If == category info, Just (expr, clauses) <- uncons children =
      case clauses of
        [clause1, clause2] -> pure $ S.If expr clause1 (Just clause2)
        [clause] -> pure $ S.If expr clause Nothing
        _ -> errorWith children
    construct children | For == category info, Just (exprs, body) <- unsnoc children =
      pure $ S.For exprs body
    construct children | While == category info, [expr, body] <- children =
      pure $ S.While expr body
    construct children | DoWhile == category info, [expr, body] <- children =
      pure $ S.DoWhile expr body
    construct children | Throw == category info, [expr] <- children =
      pure $ S.Throw expr
    construct children | Constructor == category info, [expr] <- children =
      pure $ S.Constructor expr
    construct children | Try == category info = case children of
      [body] -> pure $ S.Try body Nothing Nothing
      [body, catch] | Catch <- category (extract catch) -> pure $ S.Try body (Just catch) Nothing
      [body, finally] | Finally <- category (extract finally) -> pure $ S.Try body Nothing (Just finally)
      [body, catch, finally] | Catch <- category (extract catch),
                               Finally <- category (extract finally) ->
        pure $ S.Try body (Just catch) (Just finally)
      _ -> errorWith children
    construct children | ArrayLiteral == category info =
      pure $ S.Array children
    construct children | Method == category info = case children of
      [identifier, params, exprs] |
        Params == category (extract params),
        S.Indexed params' <- unwrap params ->
          pure $ S.Method identifier params' (toList (unwrap exprs))
      [identifier, exprs] ->
        pure $ S.Method identifier mempty (toList (unwrap exprs))
      _ -> errorWith children
    construct children | Class == category info = case children of
      [identifier, superclass, definitions] ->
        pure $ S.Class identifier (Just superclass) (toList (unwrap definitions))
      [identifier, definitions] ->
        pure $ S.Class identifier Nothing (toList (unwrap definitions))
      _ -> errorWith children
    construct children =
      pure $ S.Indexed children

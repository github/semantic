module Parser where

import Prologue hiding (Constructor)
import Data.Record
import Data.Text (pack)
import Category
import Info
import Range
import qualified Syntax as S
import Term
import qualified Data.Set as Set
import Source

-- | A function that takes a source file and returns an annotated AST.
-- | The return is in the IO monad because some of the parsers are written in C
-- | and aren't pure.
type Parser f a = Source Char -> IO (Cofree f a)

-- | Categories that are treated as fixed nodes.
fixedCategories :: Set.Set Category
fixedCategories = Set.fromList [ BinaryOperator, Pair ]

-- | Should these categories be treated as fixed nodes?
isFixed :: Category -> Bool
isFixed = flip Set.member fixedCategories

-- | Given a function that maps production names to sets of categories, produce
-- | a Constructor.
termConstructor :: (Show (Record fields), HasField fields Category, HasField fields Range) => Source Char -> (Record fields) -> [Term Text (Record fields)] -> Term Text (Record fields)
termConstructor source info = cofree . construct
  where
    withDefaultInfo syntax = (info :< syntax)
    construct [] = withDefaultInfo . S.Leaf . pack . toString $ slice (characterRange info) source
    construct children | Assignment == category info = case children of
      (identifier:value:[]) -> withDefaultInfo $ S.Assignment identifier value
    construct children | MathAssignment == category info = case children of
      (identifier:value:[]) -> withDefaultInfo $ S.MathAssignment identifier value
    construct children | MemberAccess == category info = case children of
      (base:property:[]) -> withDefaultInfo $ S.MemberAccess base property
    construct children | SubscriptAccess == category info = case children of
      (base:element:[]) -> withDefaultInfo $ S.SubscriptAccess base element
    construct children | Operator == category info = withDefaultInfo $ S.Operator children
    construct children | Function == category info = withDefaultInfo $ case children of
      (body:[]) -> S.Function Nothing Nothing body
      (params:body:[]) | (info :< _) <- runCofree params, Params == category info ->
        S.Function Nothing (Just params) body
      (id:body:[]) | (info :< _) <- runCofree id, Identifier == category info ->
        S.Function (Just id) Nothing body
      (id:params:body:[]) | (info :< _) <- runCofree id, Identifier == category info ->
        S.Function (Just id) (Just params) body
      x -> error $ "Expected a function declaration but got: " <> show x

    construct children | FunctionCall == category info = case runCofree <$> children of
      [ (_ :< S.MemberAccess{..}), params@(_ :< S.Args{}) ] ->
        setCategory info MethodCall :< S.MethodCall memberId property (cofree params)
      (x:xs) ->
        withDefaultInfo $ S.FunctionCall (cofree x) (cofree <$> xs)

    construct children | Ternary == category info = case children of
      (condition:cases) -> withDefaultInfo $ S.Ternary condition cases

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

    construct children | isFixed (category info) = withDefaultInfo $ S.Fixed children
    construct children =
      withDefaultInfo $ S.Indexed children

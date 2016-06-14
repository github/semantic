module Parser where

import Prologue hiding (Constructor)
import Data.Text (pack)
import Category
import Info
import Syntax as S
import Term
import qualified Data.OrderedMap as Map
import qualified Data.Set as Set
import Source

-- | A function that takes a source file and returns an annotated AST.
-- | The return is in the IO monad because some of the parsers are written in C
-- | and aren't pure.
type Parser = Source Char -> IO (Term Text Info)

-- | A function which constructs a term from a source string, annotation, and children.
type Constructor = Source Char -> Info -> [Term Text Info] -> Term Text Info

-- | Categories that are treated as keyed nodes.
keyedCategories :: Set.Set Category
keyedCategories = Set.fromList [ DictionaryLiteral ]

-- | Categories that are treated as fixed nodes.
fixedCategories :: Set.Set Category
fixedCategories = Set.fromList [ BinaryOperator, Pair ]

-- | Should these categories be treated as keyed nodes?
isKeyed :: Category -> Bool
isKeyed = flip Set.member keyedCategories

-- | Should these categories be treated as fixed nodes?
isFixed :: Category -> Bool
isFixed = flip Set.member fixedCategories

-- | Given a function that maps production names to sets of categories, produce
-- | a Constructor.
termConstructor :: Constructor
termConstructor source info = cofree . construct
  where
    withDefaultInfo syntax = (info :< syntax)
    construct :: [Term Text Info] -> CofreeF (Syntax Text) Info (Term Text Info)
    construct [] = withDefaultInfo $ Leaf . pack . toString $ slice (characterRange info) source
    construct children | Assignment == category info = case children of
      (identifier:value:[]) -> withDefaultInfo $ S.Assignment identifier value
    construct children | MemberAccess == category info = case children of
      (base:property:[]) -> withDefaultInfo $ S.MemberAccess base property
    construct children | Function == category info = case children of
      (body:[]) -> withDefaultInfo $ S.Function Nothing Nothing body
      (params:body:[]) | (info :< _) <- runCofree params, Params == category info -> withDefaultInfo $ S.Function Nothing (Just params) body
      (id:body:[]) | (info :< _) <- runCofree id, Identifier == category info -> withDefaultInfo $ S.Function (Just id) Nothing body
      (id:params:body:[]) | (info :< _) <- runCofree id, Identifier == category info -> withDefaultInfo $ S.Function (Just id) (Just params) body
      x -> error $ "Expected a function declaration but got: " <> show x

    construct children | isFunctionCall (category info) = case runCofree <$> children of
      [ (_ :< S.MemberAccess{..}), params@(_ :< S.Args{}) ] -> info { category = MethodCall } :< S.MethodCall memberId property (cofree params)
      (x:xs) -> withDefaultInfo $ S.FunctionCall (cofree x) (cofree <$> xs)
    construct children | Args == category info = withDefaultInfo $ S.Args children
    construct children | isFixed (category info) = withDefaultInfo $ Fixed children
    construct children | isKeyed (category info) = withDefaultInfo . Keyed . Map.fromList $ assignKey <$> children
    construct children = withDefaultInfo $ Indexed children
    assignKey node = case runCofree node of
      info :< Fixed (key : _) | Pair == category info -> (getSubstring key, node)
      _ -> (getSubstring node, node)
    getSubstring term = pack . toString $ slice (characterRange (extract term)) source

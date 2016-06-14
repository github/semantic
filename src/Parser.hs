module Parser where

import Prologue hiding (Constructor)
import Data.Text (pack)
import Category
import Info
import Syntax
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

-- | Categories that are treated as functionCall nodes.
functionCallCategories :: Set.Set Category
functionCallCategories = Set.singleton Category.FunctionCall

-- | Should these categories be treated as keyed nodes?
isKeyed :: Category -> Bool
isKeyed = flip Set.member keyedCategories

-- | Should these categories be treated as fixed nodes?
isFixed :: Category -> Bool
isFixed = flip Set.member fixedCategories

isFunctionCall :: Category -> Bool
isFunctionCall = flip Set.member functionCallCategories

isFunction :: Category -> Bool
isFunction = flip Set.member (Set.singleton Category.Function)

isSymbol :: Category -> Bool
isSymbol = flip Set.member (Set.singleton SymbolLiteral)

isIdentifier :: Category -> Bool
isIdentifier = flip Set.member (Set.singleton Identifier)

isParams :: Category -> Bool
isParams = flip Set.member (Set.singleton Params)

isExpressions :: Category -> Bool
isExpressions = flip Set.member (Set.singleton ExpressionStatements)

isAssignment :: Category -> Bool
isAssignment = flip Set.member (Set.singleton Category.Assignment)

isMemberAccess :: Category -> Bool
isMemberAccess = flip Set.member (Set.singleton Category.MemberAccess)

isArgs :: Category -> Bool
isArgs = flip Set.member (Set.singleton Category.Args)

-- | Given a function that maps production names to sets of categories, produce
-- | a Constructor.
termConstructor :: Constructor
termConstructor source info = cofree . construct
  where
    withDefaultInfo syntax = (info :< syntax)
    construct :: [Term Text Info] -> CofreeF (Syntax Text) Info (Term Text Info)
    construct [] = withDefaultInfo $ Leaf . pack . toString $ slice (characterRange info) source
    construct children | isAssignment (category info) = case children of
      (identifier:value:[]) -> withDefaultInfo $ Syntax.Assignment identifier value
    construct children | isMemberAccess (category info) = case children of
      (base:property:[]) -> withDefaultInfo $ Syntax.MemberAccess base property
    construct children | isFunction (category info) = case children of
      (body:[]) -> withDefaultInfo $ Syntax.Function Nothing Nothing body
      (params:body:[]) | (info :< _) <- runCofree params, isParams (category info) -> withDefaultInfo $ Syntax.Function Nothing (Just params) body
      (id:body:[]) | (info :< _) <- runCofree id, isIdentifier (category info) -> withDefaultInfo $ Syntax.Function (Just id) Nothing body
      (id:params:body:[]) | (info :< _) <- runCofree id, isIdentifier (category info) -> withDefaultInfo $ Syntax.Function (Just id) (Just params) body
      x -> error $ "Expected a function declaration but got: " <> show x

    construct children | isFunctionCall (category info) = case runCofree <$> children of
      [ (_ :< Syntax.MemberAccess{..}), params@(_ :< Syntax.Args{}) ] -> (info { category = Category.MethodCall } :< Syntax.MethodCall memberId property (cofree params))
      (x:xs) -> withDefaultInfo $ Syntax.FunctionCall (cofree x) (cofree <$> xs)
    construct children | isArgs (category info) = withDefaultInfo $ Syntax.Args children
    construct children | isFixed (category info) = withDefaultInfo $ Fixed children
    construct children | isKeyed (category info) = withDefaultInfo . Keyed . Map.fromList $ assignKey <$> children
    construct children = withDefaultInfo $ Indexed children
    assignKey node = case runCofree node of
      info :< Fixed (key : _) | Pair == category info -> (getSubstring key, node)
      _ -> (getSubstring node, node)
    getSubstring term = pack . toString $ slice (characterRange (extract term)) source

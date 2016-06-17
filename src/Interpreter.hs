module Interpreter (Comparable, DiffConstructor, diffTerms) where

import Algorithm
import Data.Functor.Foldable
import Data.Functor.Both
import qualified Data.OrderedMap as Map
import qualified Data.List as List
import Data.List ((\\))
import Data.OrderedMap ((!))
import Diff
import Operation
import Patch
import Prologue hiding (lookup)
import SES
import Syntax
import Term
import Data.Align
import Data.These

-- | Returns whether two terms are comparable
type Comparable a annotation = Term a annotation -> Term a annotation -> Bool

-- | Constructs a diff from the CofreeF containing its annotation and syntax. This function has the opportunity to, for example, cache properties in the annotation.
type DiffConstructor leaf annotation = CofreeF (Syntax leaf) (Both annotation) (Diff leaf annotation) -> Diff leaf annotation

-- | Diff two terms, given a function that determines whether two terms can be compared and a cost function.
diffTerms :: (Show a, Show annotation, Eq a, Eq annotation) => DiffConstructor a annotation -> Comparable a annotation -> Cost (Diff a annotation) -> Term a annotation -> Term a annotation -> Diff a annotation
diffTerms construct comparable cost a b = fromMaybe (pure $ Replace a b) $ constructAndRun construct comparable cost a b

-- | Constructs an algorithm and runs it
constructAndRun :: (Show a, Show annotation, Eq a, Eq annotation) => DiffConstructor a annotation -> Comparable a annotation -> Cost (Diff a annotation) -> Term a annotation -> Term a annotation -> Maybe (Diff a annotation)
constructAndRun _ comparable _ a b | not $ comparable a b = Nothing

constructAndRun construct _ _ a b | (() <$ a) == (() <$ b) = hylo construct runCofree <$> zipTerms a b

constructAndRun construct comparable cost t1 t2 =
  run construct comparable cost $ algorithm a b where
    algorithm (Indexed a') (Indexed b') = free . Free $ ByIndex a' b' (annotate . Indexed)
    algorithm (Keyed a') (Keyed b') = free . Free $ ByKey a' b' (annotate . Keyed)
    algorithm (Leaf a') (Leaf b') | a' == b' = annotate $ Leaf b'
    algorithm a' b' = free . Free $ Recursive (cofree (annotation1 :< a')) (cofree (annotation2 :< b')) pure
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = pure . construct . (both annotation1 annotation2 :<)

-- | Runs the diff algorithm
run :: (Show a, Eq a, Show annotation, Eq annotation) => DiffConstructor a annotation -> Comparable a annotation -> Cost (Diff a annotation) -> Algorithm a annotation (Diff a annotation) -> Maybe (Diff a annotation)
run construct comparable cost algorithm = case runFree algorithm of
  Pure diff -> Just diff
  Free (Recursive t1 t2 f) -> run construct comparable cost . f $ recur a b where
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = construct . (both annotation1 annotation2 :<)
    diffTerms' = diffTerms construct comparable cost

    recur (Indexed a') (Indexed b') | length a' == length b' = annotate . Indexed $ zipWith diffTerms' a' b'
    recur (Fixed a') (Fixed b') | length a' == length b' = annotate . Fixed $ zipWith diffTerms' a' b'
    recur (Keyed a') (Keyed b') | Map.keys a' == bKeys = annotate . Keyed . Map.fromList . fmap repack $ bKeys where
      bKeys = Map.keys b'
      repack key = (key, interpretInBoth key a' b')
      interpretInBoth key x y = diffTerms' (x ! key) (y ! key)
    recur (FunctionCall a' as') (FunctionCall b' bs') | length as' == length bs' = annotate $ FunctionCall (diffTerms' a' b') (zipWith diffTerms' as' bs')
    recur (Function a' as' aExprs') (Function b' bs' bExprs') = annotate $ Function (liftA2 diffTerms' a' b') (liftA2 diffTerms' as' bs') (diffTerms' aExprs' bExprs')
    recur (Assignment a' as') (Assignment b' bs') = annotate $ Assignment (diffTerms' a' b') (diffTerms' as' bs')
    recur (MathAssignment a' as') (MathAssignment b' bs') = annotate $ MathAssignment (diffTerms' a' b') (diffTerms' as' bs')
    recur (MemberAccess a' as') (MemberAccess b' bs') = annotate $ MemberAccess (diffTerms' a' b') (diffTerms' as' bs')
    recur (SubscriptAccess a' as') (SubscriptAccess b' bs') = annotate $ SubscriptAccess (diffTerms' a' b') (diffTerms' as' bs')
    recur (MethodCall a' as' aParams') (MethodCall b' bs' bParams') = annotate $ MethodCall (diffTerms' a' b') (diffTerms' as' bs') (diffTerms' aParams' bParams')
    recur (Ternary aCondition' aCases') (Ternary bCondition' bCases') = annotate $ Ternary (diffTerms' aCondition' bCondition') (zipWith diffTerms' aCases' bCases')
    recur (Args as') (Args bs') = annotate . Args $ zipWith diffTerms' as' bs'
    recur (VarDecl a') (VarDecl b') = annotate . VarDecl $ diffTerms' a' b'
    recur (VarAssignment a' as') (VarAssignment b' bs') = annotate $ VarAssignment (diffTerms' a' b') (diffTerms' as' bs')
    recur (Operator a') (Operator b') = annotate $ Operator (zipWith diffTerms' a' b')
    recur (Switch a' as') (Switch b' bs') = annotate $ Switch (diffTerms' a' b') (alignWith (these (pure . Delete) (pure . Insert) diffTerms') as' bs')
    recur (Case a' as') (Case b' bs') = annotate $ Case (diffTerms' a' b') (diffTerms' as' bs')
    recur (Leaf _) (Leaf _) = pure $ Replace (cofree (annotation1 :< a)) (cofree (annotation2 :< b))
    recur _ _ = error $ "Unimplemented Interpreter.run term comparison between a: " <> show a <> "\nb: " <> show b

  Free (ByKey a b f) -> run construct comparable cost $ f byKey where
    byKey = Map.fromList $ toKeyValue <$> List.union aKeys bKeys
    toKeyValue key | key `List.elem` deleted = (key, pure . Delete $ a ! key)
    toKeyValue key | key `List.elem` inserted = (key, pure . Insert $ b ! key)
    toKeyValue key = (key, diffTerms construct comparable cost (a ! key) (b ! key))
    aKeys = Map.keys a
    bKeys = Map.keys b
    deleted = aKeys \\ bKeys
    inserted = bKeys \\ aKeys

  Free (ByIndex a b f) -> run construct comparable cost . f $ ses (constructAndRun construct comparable cost) cost a b

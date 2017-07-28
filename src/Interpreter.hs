{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Interpreter
( diffTerms
, decoratingWith
, diffTermsWith
, comparableByConstructor
) where

import Algorithm
import Control.Comonad (extract)
import Control.Comonad.Cofree (unwrap)
import Control.Monad.Free (cutoff)
import Control.Monad.Free.Freer hiding (cutoff)
import Data.Align.Generic
import Data.Functor.Both
import Data.Functor.Classes (Eq1)
import Data.Hashable (Hashable)
import Data.Maybe (isJust)
import Data.Record
import Data.Text (Text)
import Data.These
import Diff
import Info hiding (Return)
import Patch (inserting, deleting, replacing, patchSum)
import RWS
import Syntax as S hiding (Return)
import Term


-- | Diff two terms recursively, given functions characterizing the diffing.
diffTerms :: HasField fields Category
          => Both (SyntaxTerm fields) -- ^ A pair of terms representing the old and new state, respectively.
          -> SyntaxDiff fields
diffTerms = decoratingWith getLabel (diffTermsWith algorithmWithTerms comparableByCategory)

-- | Diff two terms by decorating with feature vectors computed using the supplied labelling algebra, and stripping the feature vectors from the resulting diff.
decoratingWith :: (Hashable label, Traversable f)
               => (forall a. TermF f (Record fields) a -> label)
               -> (Both (Term f (Record (FeatureVector ': fields))) -> Diff f (Record (FeatureVector ': fields)))
               -> Both (Term f (Record fields))
               -> Diff f (Record fields)
decoratingWith getLabel differ = stripDiff . differ . fmap (defaultFeatureVectorDecorator getLabel)

-- | Diff a pair of terms recurisvely, using the supplied continuation and 'ComparabilityRelation'.
diffTermsWith :: forall f fields . (Traversable f, GAlign f, Eq1 f, HasField fields FeatureVector)
              => (Term f (Record fields) -> Term f (Record fields) -> Algorithm (Term f (Record fields)) (Diff f (Record fields)) (Diff f (Record fields))) -- ^ A function producing syntax-directed continuations of the algorithm.
              -> ComparabilityRelation f fields -- ^ A relation on terms used to determine comparability and equality.
              -> Both (Term f (Record fields)) -- ^ A pair of terms.
              -> Diff f (Record fields) -- ^ The resulting diff.
diffTermsWith refine comparable (Join (a, b)) = runFreer decompose (diff a b)
  where decompose :: AlgorithmF (Term f (Record fields)) (Diff f (Record fields)) result -> Algorithm (Term f (Record fields)) (Diff f (Record fields)) result
        decompose step = case step of
          Diff t1 t2 -> refine t1 t2
          Linear t1 t2 -> case galignWith diffThese (unwrap t1) (unwrap t2) of
            Just result -> wrap . (both (extract t1) (extract t2) :<) <$> sequenceA result
            _ -> byReplacing t1 t2
          RWS as bs -> traverse diffThese (rws (editDistanceUpTo defaultM) comparable as bs)
          Delete a -> pure (deleting a)
          Insert b -> pure (inserting b)
          Replace a b -> pure (replacing a b)

-- | Compute the label for a given term, suitable for inclusion in a _p_,_q_-gram.
getLabel :: HasField fields Category => TermF Syntax (Record fields) a -> (Category, Maybe Text)
getLabel (h :< t) = (Info.category h, case t of
  Leaf s -> Just s
  _ -> Nothing)


-- | Construct an algorithm to diff a pair of terms.
algorithmWithTerms :: SyntaxTerm fields
                   -> SyntaxTerm fields
                   -> Algorithm (SyntaxTerm fields) (SyntaxDiff fields) (SyntaxDiff fields)
algorithmWithTerms t1 t2 = case (unwrap t1, unwrap t2) of
  (Indexed a, Indexed b) ->
    annotate . Indexed <$> byRWS a b
  (S.Module idA a, S.Module idB b) ->
    (annotate .) . S.Module <$> linearly idA idB <*> byRWS a b
  (S.FunctionCall identifierA typeParamsA argsA, S.FunctionCall identifierB typeParamsB argsB) -> fmap annotate $
    S.FunctionCall <$> linearly identifierA identifierB
                   <*> byRWS typeParamsA typeParamsB
                   <*> byRWS argsA argsB
  (S.Switch exprA casesA, S.Switch exprB casesB) -> fmap annotate $
    S.Switch <$> byRWS exprA exprB
             <*> byRWS casesA casesB
  (S.Object tyA a, S.Object tyB b) -> fmap annotate $
    S.Object <$> diffMaybe tyA tyB
             <*> byRWS a b
  (Commented commentsA a, Commented commentsB b) -> fmap annotate $
    Commented <$> byRWS commentsA commentsB
              <*> diffMaybe a b
  (Array tyA a, Array tyB b) -> fmap annotate $
    Array <$> diffMaybe tyA tyB
          <*> byRWS a b
  (S.Class identifierA clausesA expressionsA, S.Class identifierB clausesB expressionsB) -> fmap annotate $
    S.Class <$> linearly identifierA identifierB
            <*> byRWS clausesA clausesB
            <*> byRWS expressionsA expressionsB
  (S.Method clausesA identifierA receiverA paramsA expressionsA, S.Method clausesB identifierB receiverB paramsB expressionsB) -> fmap annotate $
    S.Method <$> byRWS clausesA clausesB
             <*> linearly identifierA identifierB
             <*> diffMaybe receiverA receiverB
             <*> byRWS paramsA paramsB
             <*> byRWS expressionsA expressionsB
  (S.Function idA paramsA bodyA, S.Function idB paramsB bodyB) -> fmap annotate $
    S.Function <$> linearly idA idB
               <*> byRWS paramsA paramsB
               <*> byRWS bodyA bodyB
  _ -> linearly t1 t2
  where
    annotate = wrap . (both (extract t1) (extract t2) :<)


-- | Test whether two terms are comparable by their Category.
comparableByCategory :: HasField fields Category => ComparabilityRelation f fields
comparableByCategory (a :< _) (b :< _) = category a == category b

-- | Test whether two terms are comparable by their constructor.
comparableByConstructor :: GAlign f => ComparabilityRelation f fields
comparableByConstructor (_ :< a) (_ :< b) = isJust (galign a b)


-- | How many nodes to consider for our constant-time approximation to tree edit distance.
defaultM :: Integer
defaultM = 10

-- | Return an edit distance as the sum of it's term sizes, given an cutoff and a syntax of terms 'f a'.
-- | Computes a constant-time approximation to the edit distance of a diff. This is done by comparing at most _m_ nodes, & assuming the rest are zero-cost.
editDistanceUpTo :: (GAlign f, Foldable f, Functor f) => Integer -> These (Term f (Record fields)) (Term f (Record fields)) -> Int
editDistanceUpTo m = these termSize termSize (\ a b -> diffSum (patchSum termSize) (cutoff m (approximateDiff a b)))
  where diffSum patchCost = sum . fmap (maybe 0 patchCost)
        approximateDiff a b = maybe (replacing a b) wrap (galignWith (these deleting inserting approximateDiff) (unwrap a) (unwrap b))

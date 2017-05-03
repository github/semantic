{-# LANGUAGE GADTs, RankNTypes #-}
module Interpreter (diffTerms, runAlgorithm, runAlgorithmSteps, runAlgorithmStep) where

import Algorithm
import Control.Monad.Free.Freer
import Data.Align.Generic
import Data.Functor.Both
import RWS
import Data.Record
import Data.These
import Diff
import Info hiding (Return)
import Patch (inserting, deleting, replacing, patchSum)
import Prologue hiding (lookup)
import Syntax as S hiding (Return)
import Term

-- | Diff two terms recursively, given functions characterizing the diffing.
diffTerms :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
  => SyntaxTerm leaf fields -- ^ A term representing the old state.
  -> SyntaxTerm leaf fields -- ^ A term representing the new state.
  -> SyntaxDiff leaf fields
diffTerms = (runAlgorithm .) . diff

-- | Run an Algorithm to completion, returning its result.
runAlgorithm :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
    => Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result
    -> result
runAlgorithm = iterFreer (\ algorithm cont -> cont (runAlgorithm (decompose algorithm)))

-- | Run an Algorithm to completion, returning the list of steps taken.
runAlgorithmSteps :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
    => Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result
    -> [Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result]
runAlgorithmSteps algorithm = case runAlgorithmStep algorithm of
  Return a -> [Return a]
  next -> next : runAlgorithmSteps next

-- | Run a single step of an Algorithm, returning its result if it has finished, or the next step otherwise.
runAlgorithmStep :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
        => Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result
        -> Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result
runAlgorithmStep step = case step of
  Return a -> Return a
  algorithm `Then` cont -> decompose algorithm >>= cont


-- | Decompose a step of an algorithm into the next steps to perform.
decompose :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
          => AlgorithmF (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result -- ^ The step in an algorithm to decompose into its next steps.
          -> Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result -- ^ The sequence of next steps to undertake to continue the algorithm.
decompose step = case step of
  Diff t1 t2 -> algorithmWithTerms t1 t2
  Linear t1 t2 -> case galignWith diffThese (unwrap t1) (unwrap t2) of
    Just result -> wrap . (both (extract t1) (extract t2) :<) <$> sequenceA result
    _ -> byReplacing t1 t2
  RWS as bs -> traverse diffThese (rws (editDistanceUpTo defaultM) comparable as bs)
  Delete a -> pure (deleting a)
  Insert b -> pure (inserting b)
  Replace a b -> pure (replacing a b)


-- | Construct an algorithm to diff a pair of terms.
algorithmWithTerms :: SyntaxTerm leaf fields
                   -> SyntaxTerm leaf fields
                   -> Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) (SyntaxDiff leaf fields)
algorithmWithTerms t1 t2 = maybe (linearly t1 t2) (fmap annotate) $ case (unwrap t1, unwrap t2) of
  (Indexed a, Indexed b) ->
    Just $ Indexed <$> byRWS a b
  (S.Module idA a, S.Module idB b) ->
    Just $ S.Module <$> linearly idA idB <*> byRWS a b
  (S.FunctionCall identifierA typeParamsA argsA, S.FunctionCall identifierB typeParamsB argsB) -> Just $
    S.FunctionCall <$> linearly identifierA identifierB
                   <*> byRWS typeParamsA typeParamsB
                   <*> byRWS argsA argsB
  (S.Switch exprA casesA, S.Switch exprB casesB) -> Just $
    S.Switch <$> byRWS exprA exprB
             <*> byRWS casesA casesB
  (S.Object tyA a, S.Object tyB b) -> Just $
    S.Object <$> maybeLinearly tyA tyB
             <*> byRWS a b
  (Commented commentsA a, Commented commentsB b) -> Just $
    Commented <$> byRWS commentsA commentsB
              <*> maybeLinearly a b
  (Array tyA a, Array tyB b) -> Just $
    Array <$> maybeLinearly tyA tyB
          <*> byRWS a b
  (S.Class identifierA clausesA expressionsA, S.Class identifierB clausesB expressionsB) -> Just $
    S.Class <$> linearly identifierA identifierB
            <*> byRWS clausesA clausesB
            <*> byRWS expressionsA expressionsB
  (S.Method clausesA identifierA receiverA paramsA expressionsA, S.Method clausesB identifierB receiverB paramsB expressionsB) -> Just $
    S.Method <$> byRWS clausesA clausesB
             <*> linearly identifierA identifierB
             <*> maybeLinearly receiverA receiverB
             <*> byRWS paramsA paramsB
             <*> byRWS expressionsA expressionsB
  (S.Function idA paramsA bodyA, S.Function idB paramsB bodyB) -> Just $
    S.Function <$> linearly idA idB
               <*> byRWS paramsA paramsB
               <*> byRWS bodyA bodyB
  _ -> Nothing
  where
    annotate = wrap . (both (extract t1) (extract t2) :<)

    maybeLinearly a b = case (a, b) of
      (Just a, Just b) -> Just <$> linearly a b
      (Nothing, Just b) -> Just <$> byInserting b
      (Just a, Nothing) -> Just <$> byDeleting a
      (Nothing, Nothing) -> pure Nothing


-- | Test whether two terms are comparable.
comparable :: (Functor f, HasField fields Category) => Term f (Record fields) -> Term f (Record fields) -> Bool
comparable = (==) `on` category . extract


-- | How many nodes to consider for our constant-time approximation to tree edit distance.
defaultM :: Integer
defaultM = 10

-- | Return an edit distance as the sum of it's term sizes, given an cutoff and a syntax of terms 'f a'.
-- | Computes a constant-time approximation to the edit distance of a diff. This is done by comparing at most _m_ nodes, & assuming the rest are zero-cost.
editDistanceUpTo :: (GAlign f, Foldable f, Functor f) => Integer -> These (Term f (Record fields)) (Term f (Record fields)) -> Int
editDistanceUpTo m = these termSize termSize (\ a b -> diffSum (patchSum termSize) (cutoff m (approximateDiff a b)))
  where diffSum patchCost = sum . fmap (maybe 0 patchCost)
        approximateDiff a b = maybe (replacing a b) wrap (galignWith (these deleting inserting approximateDiff) (unwrap a) (unwrap b))

{-# LANGUAGE GADTs, RankNTypes #-}
module Interpreter (diffTerms) where

import Algorithm
import Control.Applicative.Free
import Data.Align.Generic
import Data.Functor.Both
import Data.RandomWalkSimilarity as RWS
import Data.Record
import Data.These
import Diff
import Info
import Patch (Patch, inserting, deleting, replacing, patchSum)
import Prologue hiding (lookup, Pure)
import Syntax as S
import Term

-- | Diff two terms recursively, given functions characterizing the diffing.
diffTerms :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
  => SyntaxTerm leaf fields -- ^ A term representing the old state.
  -> SyntaxTerm leaf fields -- ^ A term representing the new state.
  -> SyntaxDiff leaf fields
diffTerms = (run .) . diff

run :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
    => Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result
    -> result
run algorithm = case runStep algorithm of
  Left a -> a
  Right next -> run next

runStep :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
        => Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result
        -> Either result (Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result)
runStep = \case
  Pure a -> Left a
  Ap algorithm cont -> Right $ cont <*> decompose algorithm


-- | Decompose a step of an algorithm into the next steps to perform.
decompose :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
          => AlgorithmF (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result -- ^ The step in an algorithm to decompose into its next steps.
          -> Algorithm (SyntaxTerm leaf fields) (SyntaxDiff leaf fields) result -- ^ The sequence of next steps to undertake to continue the algorithm.
decompose = \case
  Diff t1 t2 -> algorithmWithTerms t1 t2
  Linear t1 t2 -> case galignWith diffThese (unwrap t1) (unwrap t2) of
    Just result -> wrap . (both (extract t1) (extract t2) :<) <$> sequenceA result
    _ -> byReplacing t1 t2
  RWS as bs -> traverse diffThese (rws (editDistanceUpTo defaultM) comparable as bs)
  Delete a -> pure (deleting a)
  Insert b -> pure (inserting b)
  Replace a b -> pure (replacing a b)


-- | Construct an algorithm to diff a pair of terms.
algorithmWithTerms :: MonadFree (TermF (Syntax leaf) (Both a)) diff
                   => Term (Syntax leaf) a
                   -> Term (Syntax leaf) a
                   -> Algorithm (Term (Syntax leaf) a) (diff (Patch (Term (Syntax leaf) a))) (diff (Patch (Term (Syntax leaf) a)))
algorithmWithTerms t1 t2 = maybe (linearly t1 t2) (fmap annotate) $ case (unwrap t1, unwrap t2) of
  (Indexed a, Indexed b) ->
    Just $ Indexed <$> byRWS a b
  (S.Module idA a, S.Module idB b) ->
    Just $ S.Module <$> linearly idA idB <*> byRWS a b
  (S.FunctionCall identifierA argsA, S.FunctionCall identifierB argsB) -> Just $
    S.FunctionCall <$> linearly identifierA identifierB
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
  (S.Class identifierA paramsA expressionsA, S.Class identifierB paramsB expressionsB) -> Just $
    S.Class <$> linearly identifierA identifierB
            <*> maybeLinearly paramsA paramsB
            <*> byRWS expressionsA expressionsB
  (S.Method identifierA receiverA tyA paramsA expressionsA, S.Method identifierB receiverB tyB paramsB expressionsB) -> Just $
    S.Method <$> linearly identifierA identifierB
             <*> maybeLinearly receiverA receiverB
             <*> maybeLinearly tyA tyB
             <*> byRWS paramsA paramsB
             <*> byRWS expressionsA expressionsB
  (S.Function idA paramsA tyA bodyA, S.Function idB paramsB tyB bodyB) -> Just $
    S.Function <$> linearly idA idB
               <*> byRWS paramsA paramsB
               <*> maybeLinearly tyA tyB
               <*> byRWS bodyA bodyB
  _ -> Nothing
  where
    annotate = wrap . (both (extract t1) (extract t2) :<)

    maybeLinearly :: Applicative f => Maybe a -> Maybe a -> Algorithm a (f (Patch a)) (Maybe (f (Patch a)))
    maybeLinearly a b = sequenceA $ case (a, b) of
      (Just a, Just b) -> Just $ linearly a b
      (Nothing, Just b) -> Just $ pure (inserting b)
      (Just a, Nothing) -> Just $ pure (deleting a)
      (Nothing, Nothing) -> Nothing


-- | Test whether two terms are comparable.
comparable :: (Functor f, HasField fields Category) => Term f (Record fields) -> Term f (Record fields) -> Bool
comparable = (==) `on` category . extract


-- | How many nodes to consider for our constant-time approximation to tree edit distance.
defaultM :: Integer
defaultM = 10

-- | Return an edit distance as the sum of it's term sizes, given an cutoff and a syntax of terms 'f a'.
-- | Computes a constant-time approximation to the edit distance of a diff. This is done by comparing at most _m_ nodes, & assuming the rest are zero-cost.
editDistanceUpTo :: (GAlign f, Foldable f, Functor f, HasField fields Category) => Integer -> These (Term f (Record fields)) (Term f (Record fields)) -> Int
editDistanceUpTo m = these termSize termSize (\ a b -> diffSum (patchSum termSize) (cutoff m (approximateDiff a b)))
  where diffSum patchCost = sum . fmap (maybe 0 patchCost)
        approximateDiff a b = maybe (replacing a b) wrap (galignWith (these deleting inserting approximateDiff) (unwrap a) (unwrap b))

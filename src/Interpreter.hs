{-# LANGUAGE GADTs, RankNTypes #-}
module Interpreter (Comparable, diffTerms) where

import Algorithm
import Data.Align.Generic
import Data.Functor.Foldable
import Data.Functor.Both
import Data.RandomWalkSimilarity as RWS
import Data.Record
import Data.These
import Diff
import Info
import Patch
import Prologue hiding (lookup)
import Syntax as S
import Term

-- | Returns whether two terms are comparable
type Comparable f annotation = Term f annotation -> Term f annotation -> Bool

-- | Diff two terms recursively, given functions characterizing the diffing.
diffTerms :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
  => Comparable (Syntax leaf) (Record fields) -- ^ A function to determine whether or not two terms should even be compared.
  -> SyntaxTerm leaf fields -- ^ A term representing the old state.
  -> SyntaxTerm leaf fields -- ^ A term representing the new state.
  -> SyntaxDiff leaf fields
diffTerms comparable a b = fromMaybe (replacing a b) $ diffComparableTerms comparable a b

-- | Diff two terms recursively, given functions characterizing the diffing. If the terms are incomparable, returns 'Nothing'.
diffComparableTerms :: (Eq leaf, HasField fields Category, HasField fields (Maybe FeatureVector))
                    => Comparable (Syntax leaf) (Record fields)
                    -> SyntaxTerm leaf fields
                    -> SyntaxTerm leaf fields
                    -> Maybe (SyntaxDiff leaf fields)
diffComparableTerms comparable = recur
  where recur a b
          | (category <$> a) == (category <$> b) = hylo wrap runCofree <$> zipTerms a b
          | comparable a b = runAlgorithm recur (Just <$> algorithmWithTerms a b)
          | otherwise = Nothing

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

-- | Run an algorithm, given functions characterizing the evaluation.
runAlgorithm :: (GAlign f, HasField fields Category, Eq (f (Cofree f Category)), Traversable f, HasField fields (Maybe FeatureVector))
  => (Cofree f (Record fields) -> Cofree f (Record fields) -> Maybe (Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields))))) -- ^ A function to diff two subterms recursively, if they are comparable, or else return 'Nothing'.
  -> Algorithm (Cofree f (Record fields)) (Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields)))) a -- ^ The algorithm to run.
  -> a
runAlgorithm recur = iterAp $ \ r cont -> case r of
  Linear a b -> cont . maybe (replacing a b) (wrap . (both (extract a) (extract b) :<)) $ do
    aligned <- galign (unwrap a) (unwrap b)
    traverse (these (Just . deleting) (Just . inserting) recur) aligned
  RWS as bs -> cont (rws recur as bs)

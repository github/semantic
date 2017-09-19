{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Interpreter
( diffTerms
, decoratingWith
, diffTermsWith
, comparableByConstructor
) where

import Algorithm
import Control.Applicative (Alternative(..))
import Control.Monad.Free.Freer
import Data.Align.Generic
import Data.Functor.Classes (Eq1)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust)
import Data.Record
import Data.Text (Text)
import Diff
import Info hiding (Empty, Return)
import RWS
import Syntax as S hiding (Return)
import Term


-- | Diff two terms recursively, given functions characterizing the diffing.
diffTerms :: (HasField fields1 Category, HasField fields2 Category)
          => Term Syntax (Record fields1) -- ^ A term representing the old state.
          -> Term Syntax (Record fields2) -- ^ A term representing the new state.
          -> Diff Syntax (Record fields1) (Record fields2)
diffTerms = decoratingWith getLabel getLabel (diffTermsWith algorithmWithTerms comparableByCategory)

-- | Diff two terms by decorating with feature vectors computed using the supplied labelling algebra, and stripping the feature vectors from the resulting diff.
decoratingWith :: (Hashable label, Traversable syntax)
               => (forall a. TermF syntax (Record fields1) a -> label)
               -> (forall a. TermF syntax (Record fields2) a -> label)
               -> (Term syntax (Record (FeatureVector ': fields1)) -> Term syntax (Record (FeatureVector ': fields2)) -> Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))
               -> Term syntax (Record fields1)
               -> Term syntax (Record fields2)
               -> Diff syntax (Record fields1) (Record fields2)
decoratingWith getLabel1 getLabel2 differ t1 t2 = stripDiff (differ (defaultFeatureVectorDecorator getLabel1 t1) (defaultFeatureVectorDecorator getLabel2 t2))

-- | Diff a pair of terms recurisvely, using the supplied continuation and 'ComparabilityRelation'.
diffTermsWith :: forall syntax fields1 fields2
              .  (Eq1 syntax, GAlign syntax, Traversable syntax)
              => ( Term syntax (Record (FeatureVector ': fields1))
                -> Term syntax (Record (FeatureVector ': fields2))
                -> Algorithm
                  (Term syntax)
                  (Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))
                  (Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))) -- ^ A function producing syntax-directed continuations of the algorithm.
              -> ComparabilityRelation syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)) -- ^ A relation on terms used to determine comparability and equality.
              -> Term syntax (Record (FeatureVector ': fields1)) -- ^ A term representing the old state.
              -> Term syntax (Record (FeatureVector ': fields2)) -- ^ A term representing the new state.
              -> Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)) -- ^ The resulting diff.
diffTermsWith refine comparable t1 t2 = fromMaybe (replacing t1 t2) (runFreerM decompose (diff t1 t2))
  where decompose :: Alternative m
                  => AlgorithmF
                    (Term syntax)
                    (Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))
                    result
                  -> Algorithm
                    (Term syntax)
                    (Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))
                    (m result)
        decompose step = case step of
          Algorithm.Diff t1 t2 -> pure <$> refine t1 t2
          Linear t1 t2 -> case galignWith diffThese (unwrap t1) (unwrap t2) of
            Just result -> pure <$> (merge (extract t1, extract t2) <$> sequenceA result)
            _ -> pure <$> byReplacing t1 t2
          RWS as bs -> pure <$> traverse diffThese (rws comparable as bs)
          Delete a -> pure (pure (deleting a))
          Insert b -> pure (pure (inserting b))
          Replace a b -> pure (pure (replacing a b))
          Empty -> empty
          Alt a b -> (<|>) <$> pure (pure a) <*> pure (pure b)

-- | Compute the label for a given term, suitable for inclusion in a _p_,_q_-gram.
getLabel :: HasField fields Category => TermF Syntax (Record fields) a -> (Category, Maybe Text)
getLabel (In h t) = (Info.category h, case t of
  Leaf s -> Just s
  _ -> Nothing)


-- | Construct an algorithm to diff a pair of terms.
algorithmWithTerms :: Term Syntax ann1
                   -> Term Syntax ann2
                   -> Algorithm (Term Syntax) (Diff Syntax ann1 ann2) (Diff Syntax ann1 ann2)
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
    annotate = merge (extract t1, extract t2)


-- | Test whether two terms are comparable by their Category.
comparableByCategory :: (HasField fields1 Category, HasField fields2 Category) => ComparabilityRelation syntax (Record fields1) (Record fields2)
comparableByCategory (In a _) (In b _) = category a == category b

-- | Test whether two terms are comparable by their constructor.
comparableByConstructor :: GAlign syntax => ComparabilityRelation syntax ann1 ann2
comparableByConstructor (In _ a) (In _ b) = isJust (galign a b)

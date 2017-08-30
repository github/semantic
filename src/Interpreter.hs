{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Interpreter
( decoratingWith
, diffTermsWith
, comparableByConstructor
, equivalentTerms
) where

import Algorithm
import Control.Comonad (extract)
import Control.Comonad.Cofree (unwrap)
import Control.Monad.Free (cutoff, wrap)
import Control.Monad.Free.Freer hiding (cutoff, wrap)
import Data.Align.Generic
import Data.Functor.Both
import Data.Functor.Classes (Eq1(..))
import Data.Hashable (Hashable)
import Data.Maybe (isJust)
import Data.Record
import Data.These
import Data.Union
import qualified Data.Syntax.Declaration as Declaration
import Diff
import Patch (inserting, deleting, replacing, patchSum)
import RWS
import Term

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
              -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A predicate used to determine term equality.
              -> Both (Term f (Record fields)) -- ^ A pair of terms.
              -> Diff f (Record fields) -- ^ The resulting diff.
diffTermsWith refine comparable eqTerms (Join (a, b)) = runFreer decompose (diff a b)
  where decompose :: AlgorithmF (Term f (Record fields)) (Diff f (Record fields)) result -> Algorithm (Term f (Record fields)) (Diff f (Record fields)) result
        decompose step = case step of
          Diff t1 t2 -> refine t1 t2
          Linear t1 t2 -> case galignWith diffThese (unwrap t1) (unwrap t2) of
            Just result -> wrap . (both (extract t1) (extract t2) :<) <$> sequenceA result
            _ -> byReplacing t1 t2
          RWS as bs -> traverse diffThese (rws (editDistanceUpTo defaultM) comparable eqTerms as bs)
          Delete a -> pure (deleting a)
          Insert b -> pure (inserting b)
          Replace a b -> pure (replacing a b)

-- | Test whether two terms are comparable by their constructor.
comparableByConstructor :: (GAlign f) => ComparabilityRelation f fields
comparableByConstructor (_ :< a) (_ :< b) = isJust (galign a b)

-- | Equivalency check for terms.
equivalentTerms :: (Declaration.Method :< fs, Declaration.Function :< fs, Apply1 Functor fs, Apply1 Foldable fs, Apply1 GAlign fs, Apply1 Eq1 fs)
                => Term (Union fs) a
                -> Term (Union fs) a
                -> Bool
equivalentTerms a b | Just (Declaration.Method _ _ identifierA _ _) <- prj (unwrap a)
                    , Just (Declaration.Method _ _ identifierB _ _) <- prj (unwrap b)
                    = liftEq equivalentTerms (unwrap identifierA) (unwrap identifierB)
                    | Just (Declaration.Function _ identifierA _ _) <- prj (unwrap a)
                    , Just (Declaration.Function _ identifierB _ _) <- prj (unwrap b)
                    = liftEq equivalentTerms (unwrap identifierA) (unwrap identifierB)
                    | Just aligned <- galignWith (these (const False) (const False) equivalentTerms) (unwrap a) (unwrap b)
                    = and aligned
                    | otherwise = False

-- | How many nodes to consider for our constant-time approximation to tree edit distance.
defaultM :: Integer
defaultM = 10

-- | Return an edit distance as the sum of it's term sizes, given an cutoff and a syntax of terms 'f a'.
-- | Computes a constant-time approximation to the edit distance of a diff. This is done by comparing at most _m_ nodes, & assuming the rest are zero-cost.
editDistanceUpTo :: (GAlign f, Foldable f, Functor f) => Integer -> These (Term f (Record fields)) (Term f (Record fields)) -> Int
editDistanceUpTo m = these termSize termSize (\ a b -> diffSum (patchSum termSize) (cutoff m (approximateDiff a b)))
  where diffSum patchCost = sum . fmap (maybe 0 patchCost)
        approximateDiff a b = maybe (replacing a b) wrap (galignWith (these deleting inserting approximateDiff) (unwrap a) (unwrap b))

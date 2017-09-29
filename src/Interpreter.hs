{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Interpreter
( diffTerms
, diffSyntaxTerms
, comparableByConstructor
, equivalentTerms
) where

import Algorithm
import Control.Applicative (Alternative(..))
import Control.Monad.Free.Freer
import Data.Align.Generic
import Data.Diff
import Data.Functor.Classes
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust)
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Algebra
import qualified Data.Syntax.Declaration as Declaration
import Data.Term
import Data.Text (Text)
import Data.These
import Data.Union
import Info hiding (Empty, Return)
import RWS
import Syntax (Syntax(Leaf))


-- | Diff two Syntax terms recursively.
diffSyntaxTerms :: (HasField fields1 Category, HasField fields2 Category)
                => Term Syntax (Record fields1) -- ^ A term representing the old state.
                -> Term Syntax (Record fields2) -- ^ A term representing the new state.
                -> Diff Syntax (Record fields1) (Record fields2)
diffSyntaxTerms = decoratingWith comparableByCategory (equalTerms comparableByCategory) getLabel getLabel

-- | Diff two à la carte terms recursively.
diffTerms :: (Declaration.Method :< fs, Declaration.Function :< fs, Syntax.Context :< fs, Apply Diffable fs, Apply Foldable fs, Apply Functor fs, Apply GAlign fs, Apply Show1 fs, Apply Traversable fs)
          => Term (Union fs) (Record fields1)
          -> Term (Union fs) (Record fields2)
          -> Diff (Union fs) (Record fields1) (Record fields2)
diffTerms = decoratingWith comparableByConstructor equivalentTerms constructorNameAndConstantFields constructorNameAndConstantFields

-- | Diff two terms by decorating with feature vectors computed using the supplied labelling algebra, and stripping the feature vectors from the resulting diff.
decoratingWith :: (Hashable label, Diffable syntax, GAlign syntax, Traversable syntax)
               => ComparabilityRelation syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)) -- ^ A relation on terms used to determine comparability and equality.
               -> (Term syntax (Record (FeatureVector ': fields1)) -> Term syntax (Record (FeatureVector ': fields2)) -> Bool) -- ^ A relation used to determine term equivalence.
               -> (forall a. TermF syntax (Record fields1) a -> label)
               -> (forall a. TermF syntax (Record fields2) a -> label)
               -> Term syntax (Record fields1)
               -> Term syntax (Record fields2)
               -> Diff syntax (Record fields1) (Record fields2)
decoratingWith comparability equivalence getLabel1 getLabel2 t1 t2 = stripDiff (diffTermsWith comparability equivalence (defaultFeatureVectorDecorator getLabel1 t1) (defaultFeatureVectorDecorator getLabel2 t2))

-- | Diff a pair of terms recurisvely, using the supplied continuation and 'ComparabilityRelation'.
diffTermsWith :: forall syntax fields1 fields2
              .  (Diffable syntax, GAlign syntax, Traversable syntax)
              => ComparabilityRelation syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)) -- ^ A relation on terms used to determine comparability and equality.
              -> (Term syntax (Record (FeatureVector ': fields1)) -> Term syntax (Record (FeatureVector ': fields2)) -> Bool) -- ^ A relation used to determine term equivalence.
              -> Term syntax (Record (FeatureVector ': fields1)) -- ^ A term representing the old state.
              -> Term syntax (Record (FeatureVector ': fields2)) -- ^ A term representing the new state.
              -> Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)) -- ^ The resulting diff.
diffTermsWith comparable eqTerms t1 t2 = fromMaybe (replacing t1 t2) (runAlgorithm comparable eqTerms (diff t1 t2))

-- | Run an 'Algorithm' to completion in an 'Alternative' context using the supplied comparability & equivalence relations.
runAlgorithm :: forall syntax fields1 fields2 m result
             .  (Diffable syntax, GAlign syntax, Traversable syntax, Alternative m, Monad m)
             => ComparabilityRelation syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)) -- ^ A relation on terms used to determine comparability and equality.
             -> (Term syntax (Record (FeatureVector ': fields1)) -> Term syntax (Record (FeatureVector ': fields2)) -> Bool) -- ^ A relation used to determine term equivalence.
             -> Algorithm
               (Term syntax (Record (FeatureVector ': fields1)))
               (Term syntax (Record (FeatureVector ': fields2)))
               (Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))
               result
             -> m result
runAlgorithm comparable eqTerms = go
  where go :: forall result
           .  Algorithm
             (Term syntax (Record (FeatureVector ': fields1)))
             (Term syntax (Record (FeatureVector ': fields2)))
             (Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))
             result
           -> m result
        go = iterFreerA (\ step yield -> case step of
          Algorithm.Diff t1 t2 -> go (algorithmForTerms t1 t2) <|> pure (replacing t1 t2) >>= yield
          Linear (Term (In ann1 f1)) (Term (In ann2 f2)) -> merge (ann1, ann2) <$> galignWith (go . diffThese) f1 f2 >>= yield
          RWS as bs -> traverse (go . diffThese) (rws comparable eqTerms as bs) >>= yield
          Delete a -> yield (deleting a)
          Insert b -> yield (inserting b)
          Replace a b -> yield (replacing a b)
          Empty -> empty
          Alt a b -> yield a <|> yield b)

-- | Compute the label for a given term, suitable for inclusion in a _p_,_q_-gram.
getLabel :: HasField fields Category => TermF Syntax (Record fields) a -> (Category, Maybe Text)
getLabel (In h t) = (Info.category h, case t of
  Leaf s -> Just s
  _ -> Nothing)


-- | Test whether two terms are comparable by their Category.
comparableByCategory :: (HasField fields1 Category, HasField fields2 Category) => ComparabilityRelation syntax (Record fields1) (Record fields2)
comparableByCategory (In a _) (In b _) = category a == category b

-- | Test whether two terms are comparable by their constructor.
comparableByConstructor :: (Syntax.Context :< fs, Apply GAlign fs) => ComparabilityRelation (Union fs) ann1 ann2
comparableByConstructor (In _ u1) (In _ u2)
  | Just Syntax.Context{} <- prj u1 = True
  | Just Syntax.Context{} <- prj u2 = True
  | otherwise = isJust (galign u1 u2)

-- | Equivalency relation for terms. Equivalence is determined by functions and
-- methods with equal identifiers/names and recursively by equivalent terms with
-- identical shapes.
equivalentTerms :: (Declaration.Method :< fs, Declaration.Function :< fs, Syntax.Context :< fs, Apply Foldable fs, Apply GAlign fs)
                => Term (Union fs) ann1
                -> Term (Union fs) ann2
                -> Bool
equivalentTerms t1@(Term (In _ u1)) t2@(Term (In _ u2))
  | Just (Declaration.Method _ _ identifier1 _ _) <- prj u1
  , Just (Declaration.Method _ _ identifier2 _ _) <- prj u2
  = equivalentTerms identifier1 identifier2
  | Just (Declaration.Function _ identifier1 _ _) <- prj u1
  , Just (Declaration.Function _ identifier2 _ _) <- prj u2
  = equivalentTerms identifier1 identifier2
  | Just (Syntax.Context _ s1) <- prj u1
  , Just (Syntax.Context _ s2) <- prj u2
  = equivalentTerms s1 s2
  | Just (Syntax.Context _ s1) <- prj u1
  = equivalentTerms s1 t2
  | Just (Syntax.Context _ s2) <- prj u2
  = equivalentTerms t1 s2
  | Just aligned <- galignWith (Just . these (const False) (const False) equivalentTerms) u1 u2
  = and aligned
  | otherwise = False

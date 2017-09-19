{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Interpreter
( diffTerms
, decoratingWith
, diffTermsWith
, comparableByConstructor
, equivalentTerms
) where

import Algorithm
import Control.Applicative (Alternative(..))
import Control.Monad.Free.Freer
import Data.Align.Generic
import Data.Functor.Classes (Eq1(..))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust)
import Data.Record
import Data.Text (Text)
import Data.These
import Data.Union
import qualified Data.Syntax.Declaration as Declaration
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
diffTerms = decoratingWith getLabel getLabel (diffTermsWith comparableByCategory (equalTerms comparableByCategory))

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
              .  (Diffable syntax, Eq1 syntax, GAlign syntax, Traversable syntax)
              => ComparabilityRelation syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)) -- ^ A relation on terms used to determine comparability and equality.
              -> (Term syntax (Record (FeatureVector ': fields1)) -> Term syntax (Record (FeatureVector ': fields2)) -> Bool) -- ^ A relation used to determine term equivalence.
              -> Term syntax (Record (FeatureVector ': fields1)) -- ^ A term representing the old state.
              -> Term syntax (Record (FeatureVector ': fields2)) -- ^ A term representing the new state.
              -> Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)) -- ^ The resulting diff.
diffTermsWith comparable eqTerms t1 t2 = fromMaybe (replacing t1 t2) (go (diff t1 t2))
  where go :: (Alternative m, Monad m)
           => Algorithm
             (Term syntax)
             (Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))
             (Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))
           -> m (Diff syntax (Record (FeatureVector ': fields1)) (Record (FeatureVector ': fields2)))
        go = iterFreerA (\ step yield -> case step of
          Algorithm.Diff t1 t2 -> (go (algorithmForTerms t1 t2) <|> pure (replacing t1 t2) >>= yield)
          Linear t1 t2 -> case galignWith diffThese (unwrap t1) (unwrap t2) of
            Just result -> go (merge (extract t1, extract t2) <$> sequenceA result) >>= yield
            _ -> yield (replacing t1 t2)
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
comparableByConstructor :: GAlign syntax => ComparabilityRelation syntax ann1 ann2
comparableByConstructor (In _ a) (In _ b) = isJust (galign a b)

-- | Equivalency relation for terms. Equivalence is determined by functions and
-- methods with equal identifiers/names and recursively by equivalent terms with
-- identical shapes.
equivalentTerms :: (Declaration.Method :< fs, Declaration.Function :< fs, Apply Functor fs, Apply Foldable fs, Apply GAlign fs, Apply Eq1 fs)
                => Term (Union fs) a
                -> Term (Union fs) b
                -> Bool
equivalentTerms a b | Just (Declaration.Method _ identifierA _ _) <- prj (unwrap a)
                    , Just (Declaration.Method _ identifierB _ _) <- prj (unwrap b)
                    = liftEq equivalentTerms (unwrap identifierA) (unwrap identifierB)
                    | Just (Declaration.Function identifierA _ _) <- prj (unwrap a)
                    , Just (Declaration.Function identifierB _ _) <- prj (unwrap b)
                    = liftEq equivalentTerms (unwrap identifierA) (unwrap identifierB)
                    | Just aligned <- galignWith (these (const False) (const False) equivalentTerms) (unwrap a) (unwrap b)
                    = and aligned
                    | otherwise = False

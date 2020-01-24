{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, GADTs, RankNTypes, RecordWildCards, TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- FIXME
module Diffing.Algorithm.RWS
( rws
, Options(..)
, defaultOptions
, ComparabilityRelation
, FeatureVector
, defaultFeatureVectorDecorator
, featureVectorDecorator
, pqGramDecorator
, Gram(..)
, canCompareTerms
, equalTerms
) where

import Control.Monad.State.Strict
import Data.Diff (DiffF(..), comparing, deleting, inserting, merge)
import Data.Edit
import qualified Data.KdMap.Static as KdMap
import Data.List (sortOn)
import Data.Term as Term
import Diffing.Algorithm (Diffable(..))
import Diffing.Algorithm.RWS.FeatureVector
import Diffing.Algorithm.SES
import Prologue

-- | A relation on 'Term's, guaranteed constant-time in the size of the 'Term' by parametricity.
--
--   This is used both to determine whether two root terms can be compared in O(1), and, recursively, to determine whether two nodes are equal in O(n); thus, comparability is defined s.t. two terms are equal if they are recursively comparable subterm-wise.
type ComparabilityRelation syntax ann1 ann2 = forall a b. TermF syntax ann1 a -> TermF syntax ann2 b -> Bool

rws :: (Foldable syntax, Functor syntax, Diffable syntax)
    => ComparabilityRelation syntax (FeatureVector, ann1) (FeatureVector, ann2)
    -> (Term syntax (FeatureVector, ann1) -> Term syntax (FeatureVector, ann2) -> Bool)
    -> [Term syntax (FeatureVector, ann1)]
    -> [Term syntax (FeatureVector, ann2)]
    -> [Edit (Term syntax (FeatureVector, ann1)) (Term syntax (FeatureVector, ann2))]
rws _          _          as [] = Delete <$> as
rws _          _          [] bs = Insert <$> bs
rws canCompare _          [a] [b] = if canCompareTerms canCompare a b then [Compare a b] else [Insert b, Delete a]
rws canCompare equivalent as bs
  = ses equivalent as bs
  & mapContiguous [] []
  where Options{..} = defaultOptions

        -- Map contiguous sequences of unmapped terms separated by SES-mapped equivalencies.
        mapContiguous as bs [] = mapSimilar (reverse as) (reverse bs)
        mapContiguous as bs (first : rest) = case first of
          Delete  a   -> mapContiguous (a : as)      bs  rest
          Insert    b -> mapContiguous      as  (b : bs) rest
          Compare _ _ -> mapSimilar (reverse as) (reverse bs) <> (first : mapContiguous [] [] rest)

        -- Map comparable, mutually similar terms, inserting & deleting surrounding terms.
        mapSimilar as' bs' = go as bs
          where go as [] = Delete . snd <$> as
                go [] bs = Insert . snd <$> bs
                go [a] [b] | canCompareTerms canCompare (snd a) (snd b) = [Compare (snd a) (snd b)]
                           | otherwise = [Insert (snd b), Delete (snd a)]
                go as@((i, _) : _) ((j, b) : restB) =
                  fromMaybe (Insert b : go as restB) $ do
                    -- Look up the most similar term to b near i.
                    (i', a) <- mostSimilarMatching (\ i' a -> inRange (i, i + optionsLookaheadPlaces) i' && canCompareTerms canCompare a b) kdMapA b
                    -- Look up the most similar term to a near j.
                    (j', _) <- mostSimilarMatching (\ j' b -> inRange (j, j + optionsLookaheadPlaces) j' && canCompareTerms canCompare a b) kdMapB a
                    -- Fail out if there’s a better match for a nearby.
                    guard (j == j')
                    -- Delete any elements of as before the selected element.
                    let (deleted, _ : restA) = span ((< i') . fst) as
                    pure $! (Delete . snd <$> deleted) <> (Compare a b : go restA restB)
                (as, bs) = (zip [0..] as', zip [0..] bs')
                (kdMapA, kdMapB) = (toKdMap as, toKdMap bs)

        -- Find the most similar term matching a predicate, if any.
        --
        -- RWS can produce false positives in the case of e.g. hash collisions. Therefore, we find the _l_ nearest candidates, filter out any which don’t match the predicate, and select the minimum of the remaining by (a constant-time approximation of) edit distance.
        --
        -- cf §4.2 of RWS-Diff
        mostSimilarMatching isEligible tree term = listToMaybe (sortOn (editDistanceUpTo optionsNodeComparisons term . snd) candidates)
          where candidates = filter (uncurry isEligible) (snd <$> KdMap.kNearest tree optionsMaxSimilarTerms (fst (termAnnotation term)))

data Options = Options
  { optionsLookaheadPlaces :: {-# UNPACK #-} !Int -- ^ How many places ahead should we look for similar terms?
  , optionsMaxSimilarTerms :: {-# UNPACK #-} !Int -- ^ The maximum number of similar terms to consider.
  , optionsNodeComparisons :: {-# UNPACK #-} !Int -- ^ The number of nodes to compare when selecting the most similar term.
  }

defaultOptions :: Options
defaultOptions = Options
  { optionsLookaheadPlaces = 0
  , optionsMaxSimilarTerms = 2
  , optionsNodeComparisons = 10
  }

defaultP, defaultQ :: Int
defaultP = 0
defaultQ = 3


toKdMap :: [(Int, Term syntax (FeatureVector, ann))] -> KdMap.KdMap Double FeatureVector (Int, Term syntax (FeatureVector, ann))
toKdMap = KdMap.build unFV . fmap (fst . termAnnotation . snd &&& id)

-- | A `Gram` is a fixed-size view of some portion of a tree, consisting of a `stem` of _p_ labels for parent nodes, and a `base` of _q_ labels of sibling nodes. Collectively, the bag of `Gram`s for each node of a tree (e.g. as computed by `pqGrams`) form a summary of the tree.
data Gram label = Gram { stem :: [Maybe label], base :: [Maybe label] }
 deriving (Eq, Generic, Hashable, Show)

-- | Annotates a term with a feature vector at each node, using the default values for the p, q, and d parameters.
defaultFeatureVectorDecorator :: (Hashable1 syntax, Traversable syntax)
                              => Term syntax ann
                              -> Term syntax (FeatureVector, ann)
defaultFeatureVectorDecorator = featureVectorDecorator . pqGramDecorator defaultP defaultQ

-- | Annotates a term with a feature vector at each node, parameterized by stem length, base width, and feature vector dimensions.
featureVectorDecorator :: (Foldable syntax, Functor syntax, Hashable label) => Term syntax (Gram label, ann) -> Term syntax (FeatureVector, ann)
featureVectorDecorator = cata (\ (In (label, ann) functor) ->
  termIn (foldl' addSubtermVector (unitVector (hash label)) functor, ann) functor)
  where addSubtermVector v term = addVectors v (fst (termAnnotation term))

-- | Annotates a term with the corresponding p,q-gram at each node.
pqGramDecorator :: Traversable syntax
                => Int                                    -- ^ 'p'; the desired stem length for the grams.
                -> Int                                    -- ^ 'q'; the desired base length for the grams.
                -> Term syntax ann                        -- ^ The term to decorate.
                -> Term syntax (Gram (Label syntax), ann) -- ^ The decorated term.
pqGramDecorator p q = cata algebra
  where
    algebra term = let label = Label (termFOut term) in
      termIn (gram label, termFAnnotation term) (assignParentAndSiblingLabels (termFOut term) label)
    gram label = Gram (padToSize p []) (padToSize q (pure (Just label)))
    assignParentAndSiblingLabels functor label = (`evalState` (replicate (q `div` 2) Nothing <> siblingLabels functor)) (for functor (assignLabels label))

    assignLabels :: label
                 -> Term syntax (Gram label, ann)
                 -> State [Maybe label] (Term syntax (Gram label, ann))
    assignLabels label (Term.Term (In (gram, rest) functor)) = do
      labels <- get
      put (drop 1 labels)
      pure $! termIn (gram { stem = padToSize p (Just label : stem gram), base = padToSize q labels }, rest) functor
    siblingLabels :: Traversable syntax => syntax (Term syntax (Gram label, ann)) -> [Maybe label]
    siblingLabels = foldMap (base . fst . termAnnotation)
    padToSize n list = take n (list <> repeat empty)

-- | Test the comparability of two root 'Term's in O(1).
canCompareTerms :: ComparabilityRelation syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Bool
canCompareTerms canCompare t1 t2 = canCompare (unTerm t1) (unTerm t2)

-- | Recursively test the equality of two 'Term's in O(n).
equalTerms :: Eq1 syntax => ComparabilityRelation syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Bool
equalTerms canCompare = go
  where go a b = canCompareTerms canCompare a b && liftEq go (termOut a) (termOut b)


-- | Return an edit distance between two terms, up to a certain depth.
--
--   Computes a constant-time approximation to the edit distance of a diff. This is done by comparing at most _m_ nodes, & assuming the rest are zero-cost.
editDistanceUpTo :: (Diffable syntax, Foldable syntax, Functor syntax) => Int -> Term syntax ann1 -> Term syntax ann2 -> Int
editDistanceUpTo m a b = diffCost m (approximateDiff a b)
  where diffCost = flip . cata $ \ diff m -> case diff of
          _ | m <= 0 -> 0
          Merge body -> sum (fmap ($ pred m) body)
          body -> succ (sum (fmap ($ pred m) body))
        approximateDiff a b = maybe (comparing a b) (merge (termAnnotation a, termAnnotation b)) (tryAlignWith (Just . edit deleting inserting approximateDiff) (termOut a) (termOut b))


data Label syntax where
  Label :: syntax a -> Label syntax

instance Hashable1 syntax => Hashable (Label syntax) where hashWithSalt salt (Label syntax) = liftHashWithSalt const salt syntax

instance Eq1 syntax => Eq (Label syntax) where Label a == Label b = liftEq (const (const True)) a b

instance Ord1 syntax => Ord (Label syntax) where Label a `compare` Label b = liftCompare (const (const EQ)) a b

instance Show1 syntax => Show (Label syntax) where showsPrec d (Label syntax) = liftShowsPrec (const (const id)) (const id) d syntax

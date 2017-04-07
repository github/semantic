{-# LANGUAGE GADTs, RankNTypes, DataKinds, TypeOperators, KindSignatures #-}
module RWS (RWS.run) where

import Prologue
import Control.Monad.Effect
import Control.Monad.Effect.Internal as I
import Data.Record
import Data.These
import Term
import Data.Array
import Data.Functor.Classes
import Info
import SES
import Data.Functor.Classes.Eq.Generic

import qualified Data.IntMap as IntMap
import Data.Semigroup (Min(..), Option(..))

-- rws :: (GAlign f, Traversable f, Eq1 f, HasField fields Category, HasField fields (Maybe FeatureVector))
--     => (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
--     -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
--     -> [Term f (Record fields)] -- ^ The list of old terms.
--     -> [Term f (Record fields)] -- ^ The list of new terms.
--     -> [These (Term f (Record fields)) (Term f (Record fields))] -- ^ The resulting list of similarity-matched diffs.
-- rws editDistance canCompare as bs = undefined
-- | A term which has not yet been mapped by `rws`, along with its feature vector summary & index.
data UnmappedTerm f fields = UnmappedTerm {
    termIndex :: Int -- ^ The index of the term within its root term.
  , feature   :: FeatureVector -- ^ Feature vector
  , term      :: Term f (Record fields) -- ^ The unmapped term
}

-- | Either a `term`, an index of a matched term, or nil.
data TermOrIndexOrNone term = Term term | Index Int | None

rws = do
  sesDiffs <- ses'
  (featureAs, featureBs, mappedDiffs, allDiffs) <- genFeaturizedTermsAndDiffs' sesDiffs
  nearestNeighbours <- findNearestNeighoursToDiff allDiffs (minimumTermIndex featureAs, toMap featureAs, toMap featureBs)
  remaining <- deleteRemaining nearestNeighbours mappedDiffs
  insertMapped remaining

ses' = send SES

genFeaturizedTermsAndDiffs' = send . GenFeaturizedTermsAndDiffs


data RWS f (fields :: [*]) result where
  -- RWS :: RWS a b (EditScript a b)
  SES :: RWS f fields (RWSEditScript f fields)
  -- FindNearestNeighbourToDiff :: TermOrIndexOrNone (UnmappedTerm f fields) ->
  GenFeaturizedTermsAndDiffs :: HasField fields (Maybe FeatureVector) => RWSEditScript f fields -> RWS f fields ([UnmappedTerm f fields], [UnmappedTerm f fields], [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))], [TermOrIndexOrNone (UnmappedTerm f fields)])
  -- EraseFeatureVector :: forall a b f fields. RwsF a b (EditScript (Term f (Record fields)) (Term f (Record fields)))

type FeatureVector = Array Int Double

type RWSEditScript f fields = [These (Term f (Record fields)) (Term f (Record fields))]

run :: (Eq1 f, Functor f, HasField fields Category, HasField fields FeatureVector, Foldable t) => t (Term f (Record fields)) -> t (Term f (Record fields)) -> Eff '[RWS f fields] (RWSEditScript f fields) -> RWSEditScript f fields
run _ _ (Val x) = x
run as bs (E u q) = case decompose u of
  Right SES ->
    let sesDiffs = ses (gliftEq (==) `on` fmap category) as bs in
      RWS.run as bs (apply q sesDiffs)
  Right (GenFeaturizedTermsAndDiffs sesDiffs) -> RWS.run as bs . apply q $ evalState (genFeaturizedTermsAndDiffs sesDiffs) (0, 0)

genFeaturizedTermsAndDiffs :: (Functor f, HasField fields (Maybe FeatureVector)) => RWSEditScript f fields -> State (Int, Int) ([UnmappedTerm f fields], [UnmappedTerm f fields], [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))], [TermOrIndexOrNone (UnmappedTerm f fields)])
genFeaturizedTermsAndDiffs sesDiffs = case sesDiffs of
  [] -> pure ([], [], [], [])
  (diff : diffs) -> do
    (counterA, counterB) <- get
    case diff of
      This term -> do
        put (succ counterA, counterB)
        (as, bs, mappedDiffs, allDiffs) <- genFeaturizedTermsAndDiffs diffs
        pure (as <> pure (featurize counterA term), bs, mappedDiffs, allDiffs <> pure None)
      That term -> do
        put (counterA, succ counterB)
        (as, bs, mappedDiffs, allDiffs) <- genFeaturizedTermsAndDiffs diffs
        pure (as, bs <> pure (featurize counterB term), mappedDiffs, allDiffs <> pure (Term (featurize counterB term)))
      These a b -> do
        put (succ counterA, succ counterB)
        (as, bs, mappedDiffs, allDiffs) <- genFeaturizedTermsAndDiffs diffs
        pure (as, bs, mappedDiffs <> pure (These counterA counterB, These a b), allDiffs <> pure (Index counterA))

featurize :: (HasField fields (Maybe FeatureVector), Functor f) => Int -> Term f (Record fields) -> UnmappedTerm f fields
featurize index term = UnmappedTerm index (let Just v = getField (extract term) in v) (eraseFeatureVector term)

eraseFeatureVector :: (Functor f, HasField fields (Maybe FeatureVector)) => Term f (Record fields) -> Term f (Record fields)
eraseFeatureVector term = let record :< functor = runCofree term in
  cofree (setFeatureVector record Nothing :< functor)

setFeatureVector :: HasField fields (Maybe FeatureVector) => Record fields -> Maybe FeatureVector -> Record fields
setFeatureVector = setField

minimumTermIndex = pred . maybe 0 getMin . getOption . foldMap (Option . Just . Min . termIndex)

toMap = IntMap.fromList . fmap (termIndex &&& identity)

data EditGraph a b = EditGraph { as :: !(Array Int a), bs :: !(Array Int b) }
  deriving (Eq, Show)

-- data Step a b result where
--   M :: HasCallStack => RwsF a b c -> Step a b c
--   S :: State (RwsState a b) c -> Step a b c

-- newtype RwsState a b = RwsState { unRwsState :: (Int, a, b) }

-- type Rws a b = Freer (Step a b)

-- runRWS :: HasCallStack => (a -> a) -> EditGraph a a -> Rws a a c -> c
-- runRWS eraseFeatureVector graph@(EditGraph as bs)
--   | null as, null bs = []
--   | null as = That . eraseFeatureVector <$> toList bs
--   | null bs = This . eraseFeatureVector <$> toList as
--   | otherwise = evalState (go step) (emptyStateForGraph graph)
--
-- emptyStateForGraph :: EditGraph a b -> RwsState a b
-- emptyStateForGraph (EditGraph as bs) = let (n, m) = (length as, length bs) in
--   RwsState (listArray (Diagonal (negate m), Diagonal n) (repeat (0, [])))
--
-- eraseFeatureVector :: (Functor f, HasField fields (Maybe FeatureVector)) => Term f (Record fields) -> Term f (Record fields)
-- eraseFeatureVector term = let record :< functor = runCofree term in
--   cofree (setFeatureVector record Nothing :< functor)
--
-- setFeatureVector :: HasField fields (Maybe FeatureVector) => Record fields -> Maybe FeatureVector -> Record fields
-- setFeatureVector = setField

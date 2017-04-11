{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
module RWS (rws) where

import Prologue
import Control.Monad.Effect as Eff
import Control.Monad.Effect.Internal as I
import Data.Record
import Data.These
import Term
import Data.Array
import Data.Functor.Classes
import Info
import SES
import qualified Data.Functor.Both as Both
import Data.Functor.Classes.Eq.Generic
import Data.RandomWalkSimilarity (FeatureVector)

import Data.KdTree.Static hiding (toList)
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

rws :: (HasField fields Category, HasField fields (Maybe FeatureVector), Foldable t, Functor f, Eq1 f) => (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -> t (Term f (Record fields)) -> t (Term f (Record fields)) -> RWSEditScript f fields
rws editDistance canCompare as bs = Eff.run $ RWS.run editDistance canCompare as bs rws'

rws' :: (HasField fields (Maybe FeatureVector), RWS f fields :< e) => Eff e [These (Term f (Record fields)) (Term f (Record fields))]
rws' = do
  sesDiffs <- ses'
  (featureAs, featureBs, mappedDiffs, allDiffs) <- genFeaturizedTermsAndDiffs' sesDiffs
  (diffs, remaining) <- findNearestNeighoursToDiff' allDiffs featureAs featureBs
  diffs' <- deleteRemaining' diffs remaining
  rwsDiffs <- insertMapped' mappedDiffs diffs'
  pure (fmap snd rwsDiffs)

ses' :: (HasField fields (Maybe FeatureVector), RWS f fields :< e) => Eff e (RWSEditScript f fields)
ses' = send SES

genFeaturizedTermsAndDiffs' :: (HasField fields (Maybe FeatureVector), RWS f fields :< e)
                            => RWSEditScript f fields
                            -> Eff e ([UnmappedTerm f fields], [UnmappedTerm f fields], [(These Int Int, Diff f fields)], [TermOrIndexOrNone (UnmappedTerm f fields)])
genFeaturizedTermsAndDiffs' = send . GenFeaturizedTermsAndDiffs

findNearestNeighoursToDiff' :: (RWS f fields :< e)
                            => [TermOrIndexOrNone (UnmappedTerm f fields)]
                            -> [UnmappedTerm f fields]
                            -> [UnmappedTerm f fields]
                            -> Eff e ([(These Int Int, Diff f fields)], UnmappedTerms f fields)
findNearestNeighoursToDiff' diffs as bs = send (FindNearestNeighoursToDiff diffs as bs)

deleteRemaining' diffs remaining = send (DeleteRemaining diffs remaining)

insertMapped' diffs mappedDiffs = send (InsertMapped diffs mappedDiffs)


data RWS f fields result where
  -- RWS :: RWS a b (EditScript a b)
  SES :: RWS f fields (RWSEditScript f fields)
  -- FindNearestNeighbourToDiff :: TermOrIndexOrNone (UnmappedTerm f fields) ->
  GenFeaturizedTermsAndDiffs :: HasField fields (Maybe FeatureVector) => RWSEditScript f fields -> RWS f fields ([UnmappedTerm f fields], [UnmappedTerm f fields], [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))], [TermOrIndexOrNone (UnmappedTerm f fields)])

  FindNearestNeighoursToDiff :: [TermOrIndexOrNone (UnmappedTerm f fields)] -> [UnmappedTerm f fields] -> [UnmappedTerm f fields] -> RWS f fields ([(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))], UnmappedTerms f fields)

  DeleteRemaining :: [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))] -> UnmappedTerms f fields -> RWS f fields [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))]

  InsertMapped :: [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))] -> [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))] -> RWS f fields [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))]
  -- EraseFeatureVector :: forall a b f fields. RwsF a b (EditScript (Term f (Record fields)) (Term f (Record fields)))

-- | An IntMap of unmapped terms keyed by their position in a list of terms.
type UnmappedTerms f fields = IntMap (UnmappedTerm f fields)

type RWSEditScript f fields = [These (Term f (Record fields)) (Term f (Record fields))]

run :: (Eq1 f, Functor f, HasField fields Category, HasField fields (Maybe FeatureVector), Foldable t)
    => (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
    -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
    -> t (Term f (Record fields))
    -> t (Term f (Record fields))
    -> Eff (RWS f fields ': e) (RWSEditScript f fields)
    -> Eff e (RWSEditScript f fields)
run editDistance canCompare as bs = relay pure (\m k -> case m of
  SES -> k $ ses (gliftEq (==) `on` fmap category) as bs
  (GenFeaturizedTermsAndDiffs sesDiffs) ->
    k $ genFeaturizedTermsAndDiffs sesDiffs
  (FindNearestNeighoursToDiff allDiffs featureAs featureBs) ->
    k $ findNearestNeighboursToDiff editDistance canCompare allDiffs featureAs featureBs
  (DeleteRemaining allDiffs remainingDiffs) ->
    k $ deleteRemaining allDiffs remainingDiffs
  (InsertMapped allDiffs mappedDiffs) ->
    k $ insertMapped allDiffs mappedDiffs)

type Diff f fields = These (Term f (Record fields)) (Term f (Record fields))

insertMapped :: Foldable t => t (These Int Int, Diff f fields) -> [(These Int Int, Diff f fields)] -> [(These Int Int, Diff f fields)]
insertMapped diffs into = foldl' (\into (i, mappedTerm) -> insertDiff (i, mappedTerm) into) into diffs

deleteRemaining :: (Traversable t)
                => [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))]
                -> t (RWS.UnmappedTerm f fields)
                -> [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))]
deleteRemaining diffs unmappedAs =
  foldl'
    (\into (i, deletion) -> insertDiff (This i, deletion) into)
    diffs
    ((termIndex &&& This . term) <$> unmappedAs)

-- | Inserts an index and diff pair into a list of indices and diffs.
insertDiff :: (These Int Int, These (Term f (Record fields)) (Term f (Record fields))) -> [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))] -> [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))]
insertDiff inserted [] = [ inserted ]
insertDiff a@(ij1, _) (b@(ij2, _):rest) = case (ij1, ij2) of
  (These i1 i2, These j1 j2) -> if i1 <= j1 && i2 <= j2 then a : b : rest else b : insertDiff a rest
  (This i, This j) -> if i <= j then a : b : rest else b : insertDiff a rest
  (That i, That j) -> if i <= j then a : b : rest else b : insertDiff a rest
  (This i, These j _) -> if i <= j then a : b : rest else b : insertDiff a rest
  (That i, These _ j) -> if i <= j then a : b : rest else b : insertDiff a rest

  (This _, That _) -> b : insertDiff a rest
  (That _, This _) -> b : insertDiff a rest

  (These i1 i2, _) -> case break (isThese . fst) rest of
    (rest, tail) -> let (before, after) = foldr' (combine i1 i2) ([], []) (b : rest) in
       case after of
         [] -> before <> insertDiff a tail
         _ -> before <> (a : after) <> tail
  where
    combine i1 i2 each (before, after) = case fst each of
      This j1 -> if i1 <= j1 then (before, each : after) else (each : before, after)
      That j2 -> if i2 <= j2 then (before, each : after) else (each : before, after)
      These _ _ -> (before, after)



findNearestNeighboursToDiff :: (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
                           -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
                           -> [TermOrIndexOrNone (UnmappedTerm f fields)]
                           -> [UnmappedTerm f fields]
                           -> [UnmappedTerm f fields]
                           -> ([(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))], UnmappedTerms f fields)
findNearestNeighboursToDiff editDistance canCompare allDiffs featureAs featureBs =
  let
    (diffs, (_, remaining, _)) = traverse (findNearestNeighbourToDiff' editDistance canCompare (toKdTree <$> Both.both featureAs featureBs)) allDiffs & fmap catMaybes & (`runState` (minimumTermIndex featureAs, toMap featureAs, toMap featureBs)) in
  (diffs, remaining)

findNearestNeighbourToDiff' :: (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
                           -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
                           -> Both.Both (KdTree Double (UnmappedTerm f fields))
                           -> TermOrIndexOrNone (UnmappedTerm f fields)
                           -> State (Int, UnmappedTerms f fields, UnmappedTerms f fields)
                                    (Maybe (These Int Int, These (Term f (Record fields)) (Term f (Record fields))))
findNearestNeighbourToDiff' editDistance canCompare kdTrees termThing = case termThing of
  None -> pure Nothing
  Term term -> Just <$> findNearestNeighbourTo editDistance canCompare kdTrees term
  Index i -> do
    (_, unA, unB) <- get
    put (i, unA, unB)
    pure Nothing

-- | Construct a diff for a term in B by matching it against the most similar eligible term in A (if any), marking both as ineligible for future matches.
findNearestNeighbourTo :: (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
                       -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
                       -> Both.Both (KdTree Double (UnmappedTerm f fields))
                       -> UnmappedTerm f fields
                       -> State (Int, UnmappedTerms f fields, UnmappedTerms f fields)
                                (These Int Int, These (Term f (Record fields)) (Term f (Record fields)))
findNearestNeighbourTo editDistance canCompare kdTrees term@(UnmappedTerm j _ b) = do
  (previous, unmappedA, unmappedB) <- get
  fromMaybe (insertion previous unmappedA unmappedB term) $ do
    -- Look up the nearest unmapped term in `unmappedA`.
    foundA@(UnmappedTerm i _ a) <- nearestUnmapped editDistance canCompare (IntMap.filterWithKey (\ k _ ->
      isInMoveBounds previous k)
      unmappedA) (Both.fst kdTrees) term
    -- Look up the nearest `foundA` in `unmappedB`
    UnmappedTerm j' _ _ <- nearestUnmapped editDistance canCompare unmappedB (Both.snd kdTrees) foundA
    -- Return Nothing if their indices don't match
    guard (j == j')
    guard (canCompare a b)
    pure $! do
      put (i, IntMap.delete i unmappedA, IntMap.delete j unmappedB)
      pure (These i j, These a b)

isInMoveBounds previous i = previous < i && i < previous + defaultMoveBound

-- | Finds the most-similar unmapped term to the passed-in term, if any.
--
-- RWS can produce false positives in the case of e.g. hash collisions. Therefore, we find the _l_ nearest candidates, filter out any which have already been mapped, and select the minimum of the remaining by (a constant-time approximation of) edit distance.
--
-- cf §4.2 of RWS-Diff
nearestUnmapped
  :: (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
  -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
  -> UnmappedTerms f fields -- ^ A set of terms eligible for matching against.
  -> KdTree Double (UnmappedTerm f fields) -- ^ The k-d tree to look up nearest neighbours within.
  -> UnmappedTerm f fields -- ^ The term to find the nearest neighbour to.
  -> Maybe (UnmappedTerm f fields) -- ^ The most similar unmapped term, if any.
nearestUnmapped editDistance canCompare unmapped tree key = getFirst $ foldMap (First . Just) (sortOn (editDistanceIfComparable editDistance canCompare (term key) . term) (toList (IntMap.intersection unmapped (toMap (kNearest tree defaultL key)))))

editDistanceIfComparable editDistance canCompare a b = if canCompare a b
  then editDistance (These a b)
  else maxBound

defaultD, defaultL, defaultP, defaultQ, defaultMoveBound :: Int
defaultD = 15
-- | How many of the most similar terms to consider, to rule out false positives.
defaultL = 2
defaultP = 2
defaultQ = 3
defaultMoveBound = 2

-- Returns a state (insertion index, old unmapped terms, new unmapped terms), and value of (index, inserted diff),
-- given a previous index, two sets of umapped terms, and an unmapped term to insert.
insertion :: Int
             -> UnmappedTerms f fields
             -> UnmappedTerms f fields
             -> UnmappedTerm f fields
             -> State (Int, UnmappedTerms f fields, UnmappedTerms f fields)
                      (These Int Int, These (Term f (Record fields)) (Term f (Record fields)))
insertion previous unmappedA unmappedB (UnmappedTerm j _ b) = do
  put (previous, unmappedA, IntMap.delete j unmappedB)
  pure (That j, That b)

genFeaturizedTermsAndDiffs :: (Functor f, HasField fields (Maybe FeatureVector)) => RWSEditScript f fields -> ([UnmappedTerm f fields], [UnmappedTerm f fields], [(These Int Int, These (Term f (Record fields)) (Term f (Record fields)))], [TermOrIndexOrNone (UnmappedTerm f fields)])
genFeaturizedTermsAndDiffs sesDiffs = pure (featurizedAs, featurizedBs, countersAndDiffs, allDiffs)
  where
   (featurizedAs, featurizedBs, _, _, countersAndDiffs, allDiffs) = foldl' (\(as, bs, counterA, counterB, diffs, allDiffs) diff ->
      case diff of
        This term ->
          (as <> pure (featurize counterA term), bs, succ counterA, counterB, diffs, allDiffs <> pure None)
        That term ->
          (as, bs <> pure (featurize counterB term), counterA, succ counterB, diffs, allDiffs <> pure (Term (featurize counterB term)))
        These a b ->
          (as, bs, succ counterA, succ counterB, diffs <> pure (These counterA counterB, These a b), allDiffs <> pure (Index counterA))
      ) ([], [], 0, 0, [], []) sesDiffs

featurize :: (HasField fields (Maybe FeatureVector), Functor f) => Int -> Term f (Record fields) -> UnmappedTerm f fields
featurize index term = UnmappedTerm index (let Just v = getField (extract term) in v) (eraseFeatureVector term)

eraseFeatureVector :: (Functor f, HasField fields (Maybe FeatureVector)) => Term f (Record fields) -> Term f (Record fields)
eraseFeatureVector term = let record :< functor = runCofree term in
  cofree (setFeatureVector record Nothing :< functor)

setFeatureVector :: HasField fields (Maybe FeatureVector) => Record fields -> Maybe FeatureVector -> Record fields
setFeatureVector = setField

minimumTermIndex = pred . maybe 0 getMin . getOption . foldMap (Option . Just . Min . termIndex)

toMap = IntMap.fromList . fmap (termIndex &&& identity)

toKdTree = build (elems . feature)

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

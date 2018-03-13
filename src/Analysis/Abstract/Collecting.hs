{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Collecting
( type Collecting
) where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Data.Abstract.Live
import Data.Abstract.Store
import Data.Abstract.Value
import Prologue

newtype Collecting m term value (effects :: [* -> *]) a = Collecting (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (Collecting m term value effects)
deriving instance MonadStore value (m term value effects) => MonadStore value (Collecting m term value effects)
deriving instance MonadModuleTable term value (m term value effects) => MonadModuleTable term value (Collecting m term value effects)
deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (Collecting m term value effects)


instance ( Foldable (Cell (LocationFor value))
         , MonadAnalysis term value (m term value effects)
         , Ord (LocationFor value)
         , ValueRoots (LocationFor value) value
         )
         => MonadAnalysis term value (Collecting m term value effects) where
  type RequiredEffects term value (Collecting m term value effects) = Reader (Live (LocationFor value) value) ': RequiredEffects term value (m term value effects)

  -- Small-step evaluation which garbage-collects any non-rooted addresses after evaluating each term.
  analyzeTerm term = do
    roots <- askRoots
    v <- liftAnalyze analyzeTerm term
    modifyStore (gc (roots <> valueRoots v))
    pure v


-- | 'Monad's offering a local set of 'Live' (rooted/reachable) addresses.
class Monad m => MonadGC value m where
  -- | Retrieve the local 'Live' set.
  askRoots :: m (Live (LocationFor value) value)

  -- | Run a computation with the given 'Live' set added to the local root set.
  extraRoots :: Live (LocationFor value) value -> m a -> m a

instance (Effectful m, Monad (m effects), Ord (LocationFor value), Reader (Live (LocationFor value) value) :< effects) => MonadGC value (m effects) where
  askRoots = raise ask

  extraRoots roots = raise . local (<> roots) . lower

-- | Collect any addresses in the store not rooted in or reachable from the given 'Live' set.
gc :: ( Ord (LocationFor a)
      , Foldable (Cell (LocationFor a))
      , ValueRoots (LocationFor a) a
      )
   => Live (LocationFor a) a  -- ^ The set of addresses to consider rooted.
   -> Store (LocationFor a) a -- ^ A store to collect unreachable addresses within.
   -> Store (LocationFor a) a -- ^ A garbage-collected store.
gc roots store = storeRestrict store (reachable roots store)

-- | Compute the set of addresses reachable from a given root set in a given store.
reachable :: ( Ord (LocationFor a)
             , Foldable (Cell (LocationFor a))
             , ValueRoots (LocationFor a) a
             )
          => Live (LocationFor a) a  -- ^ The set of root addresses.
          -> Store (LocationFor a) a -- ^ The store to trace addresses through.
          -> Live (LocationFor a) a  -- ^ The set of addresses reachable from the root set.
reachable roots store = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (a, as) -> go (liveInsert a seen) (case storeLookupAll a store of
            Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            _           -> seen)

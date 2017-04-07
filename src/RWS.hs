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

-- rws :: (GAlign f, Traversable f, Eq1 f, HasField fields Category, HasField fields (Maybe FeatureVector))
--     => (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
--     -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
--     -> [Term f (Record fields)] -- ^ The list of old terms.
--     -> [Term f (Record fields)] -- ^ The list of new terms.
--     -> [These (Term f (Record fields)) (Term f (Record fields))] -- ^ The resulting list of similarity-matched diffs.
-- rws editDistance canCompare as bs = undefined

data RWS f (fields :: [*]) result where
  -- RWS :: RWS a b (EditScript a b)
  SES :: RWS f fields (RWSEditScript f fields)
  -- EraseFeatureVector :: forall a b f fields. RwsF a b (EditScript (Term f (Record fields)) (Term f (Record fields)))


type FeatureVector = Array Int Double

type RWSEditScript f fields = [These (Term f (Record fields)) (Term f (Record fields))]

run :: (Eq1 f, Functor f, HasField fields Category, Foldable t) => t (Term f (Record fields)) -> t (Term f (Record fields)) -> Eff '[RWS f fields] (RWSEditScript f fields) -> RWSEditScript f fields
run _ _ (Val x) = x
run as bs (E u q) = case decompose u of
  Right SES ->
    let sesDiffs = ses (gliftEq (==) `on` fmap category) as bs in
      RWS.run as bs (apply q sesDiffs)

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

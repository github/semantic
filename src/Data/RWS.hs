{-# LANGUAGE GADTs, RankNTypes #-}
module Data.RWS (rws) where

import Prologue
import Control.Monad.Free.Freer
import Data.Record
import Data.Align.Generic
import Data.These
import Term
import Data.Array
import Data.Functor.Classes
import Info

rws :: (GAlign f, Traversable f, Eq1 f, HasField fields Category, HasField fields (Maybe FeatureVector))
    => (These (Term f (Record fields)) (Term f (Record fields)) -> Int) -- ^ A function computes a constant-time approximation to the edit distance between two terms.
    -> (Term f (Record fields) -> Term f (Record fields) -> Bool) -- ^ A relation determining whether two terms can be compared.
    -> [Term f (Record fields)] -- ^ The list of old terms.
    -> [Term f (Record fields)] -- ^ The list of new terms.
    -> [These (Term f (Record fields)) (Term f (Record fields))] -- ^ The resulting list of similarity-matched diffs.
rws editDistance canCompare as bs = undefined



data RwsF f fields result where
  RWS :: RwsF a b (EditScript a b)
  SES :: RwsF a b (EditScript a b)
  EraseFeatureVector :: forall a b f fields. RwsF a b (EditScript (Term f (Record fields)) (Term f (Record fields)))

data EditGraph a b = EditGraph { as :: !(Array Int a), bs :: !(Array Int b) }
  deriving (Eq, Show)

data Step a b result where
  M :: HasCallStack => RwsF a b c -> Step a b c
  S :: State (RwsState a b) c -> Step a b c

newtype RwsState a b = RwsState { unRwsState :: (Int, a, b) }

type Rws a b = Freer (Step a b)

runRWS :: HasCallStack => (a -> a) -> EditGraph a a -> Rws a a (EditScript a a)
runRWS eraseFeatureVector (EditGraph as bs)
  | null as = return $ That . eraseFeatureVector <$> toList bs
  | null bs = return $ This . eraseFeatureVector <$> toList as

type FeatureVector = Array Int Double

type EditScript a b = [These a b]

eraseFeatureVector :: (Functor f, HasField fields (Maybe FeatureVector)) => Term f (Record fields) -> Term f (Record fields)
eraseFeatureVector term = let record :< functor = runCofree term in
  cofree (setFeatureVector record Nothing :< functor)

setFeatureVector :: HasField fields (Maybe FeatureVector) => Record fields -> Maybe FeatureVector -> Record fields
setFeatureVector = setField

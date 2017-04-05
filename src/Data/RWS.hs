{-# LANGUAGE GADTs #-}
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



data RwsF a b result where
  RWS :: RwsF a b (EditScript a b)
  SES :: RwsF a b (EditScript a b)

data EditGraph a b = EditGraph { as :: !(Array Int a), bs :: !(Array Int b) }
  deriving (Eq, Show)

data Step a b result where
  M :: HasCallStack => RwsF a b c -> Step a b c
  S :: State (RwsState a b) c -> Step a b c

newtype RwsState a b = RwsState { unRwsState :: (Int, a, b) }

type Rws a b = Freer (Step a b)

runRWS :: HasCallStack => EditGraph a b -> Rws a b (EditScript a b)
runRWS (EditGraph as bs)
  | null as = return $ That . eraseFeatureVector <$> toList bs
  | null bs = return $ This . eraseFeatureVector <$> toList as

type FeatureVector = Array Int Double

type EditScript a b = [These a b]


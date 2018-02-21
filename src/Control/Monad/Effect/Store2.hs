{-# LANGUAGE TypeOperators, GADTs, TemplateHaskell #-}
module Control.Monad.Effect.Store2
( assign
, modify
, get
, put
, Store2
) where

import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Pointed
import Data.Semigroup

-- -- | Write a value to the given 'Address' in the 'Store'.
-- assign :: (Ord (LocationFor a), Semigroup (Cell (LocationFor a) a), Pointed (Cell (LocationFor a)), MonadStore a m) => Address (LocationFor a) a -> a -> m ()
-- assign address = modifyStore . storeInsert address

assign :: (Ord (LocationFor a), Semigroup (Cell (LocationFor a) a), Pointed (Cell (LocationFor a)), Store2 a :< es)
       => Address (LocationFor a) a
       -> a
       -> Eff es ()
assign address = modify . storeInsert address


-- -- | 'Monad's offering a readable & writable 'Store' of values for specific 'Address'es.
-- class Monad m => MonadStore a m where
--   -- | Get the current store.
--   getStore :: m (Store (LocationFor a) a)
--
--   -- | Update the current store.
--   putStore :: Store (LocationFor a) a -> m ()

-- instance (State (Store (LocationFor a) a) :< fs) => MonadStore a (Eff fs) where
--   getStore = get
--   putStore = put
type Store2 a = State (Store (LocationFor a) a)

-- data Store2 a where
--   GetStore :: Store2 (Store (LocationFor a) a)
--   PutStore :: Store (LocationFor a) a -> Store2 ()
--
-- getStore :: (Store2 :< es) => Eff es (Store (LocationFor a) a)
-- getStore = send GetStore
--
-- putStore :: (Store2 :< es) => Store (LocationFor a) a -> Eff es ()
-- putStore = send . PutStore

-- | Modify the current store using a given function.
-- modifyStore :: (Store2 :< e) => (Store (LocationFor a) a -> Store (LocationFor a) a) -> Eff e ()
-- modifyStore f = fmap f get >>= putStore


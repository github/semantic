{-# LANGUAGE DeriveFunctor, DeriveGeneric, ExistentialQuantification, LambdaCase, RankNTypes, StandaloneDeriving #-}
module Analysis.Analysis
( Analysis(..)
) where

import Control.Effect.Carrier
import Data.Text (Text)
import GHC.Generics (Generic1)

-- | A record of functions necessary to perform analysis.
--
-- This is intended to be replaced with a selection of algebraic effects providing these interfaces and carriers providing reusable implementations.
data Analysis term name address value m = Analysis
  { alloc     :: name -> m address
  , bind      :: forall a . name -> address -> m a -> m a
  , lookupEnv :: name -> m (Maybe address)
  , deref     :: address -> m (Maybe value)
  , assign    :: address -> value -> m ()
  , abstract  :: (term name -> m value) -> name -> term name -> m value
  , apply     :: (term name -> m value) -> value -> value -> m value
  , unit      :: m value
  , bool      :: Bool -> m value
  , asBool    :: value -> m Bool
  , string    :: Text -> m value
  , asString  :: value -> m Text
  , record    :: [(name, value)] -> m value
  , (...)     :: address -> name -> m (Maybe address)
  }

data Env name addr m k
  = Alloc name (addr -> m k)
  | forall a . Bind name addr (m a) (a -> m k)
  | Lookup name (Maybe addr -> m k)

deriving instance Functor m => Functor (Env name addr m)

instance HFunctor (Env name addr) where
  hmap f = \case
    Alloc name k -> Alloc name (f . k)
    Bind name addr m k -> Bind name addr (f m) (f . k)
    Lookup name k -> Lookup name (f . k)

instance Effect (Env name addr) where
  handle ctx hdl = \case
    Alloc name k -> Alloc name (hdl . (<$ ctx) . k)
    Bind name addr m k -> Bind name addr (hdl (m <$ ctx)) (hdl . fmap k)
    Lookup name k -> Lookup name (hdl . (<$ ctx) . k)


data Heap addr value m k
  = Deref addr (Maybe value -> m k)
  | Assign addr value (m k)
  deriving (Functor, Generic1)

instance HFunctor (Heap addr value)
instance Effect   (Heap addr value)


data Domain term name value m k
  -- Functions construction & elimination
  = Abstract name (term name)                 (value term name -> m k)
  | Apply (value term name) (value term name) (value term name -> m k)
  -- Unit construction (no elimination)
  | Unit (value term name -> m k)
  -- Boolean construction & elimination
  | Bool   Bool              (value term name -> m k)
  | AsBool (value term name) (Bool            -> m k)
  -- String construction & elimination
  | String   Text              (value term name -> m k)
  | AsString (value term name) (Text            -> m k)
  -- Record construction & elimination
  | Record [(name, value term name)] (value term name         -> m k)
  | Project (value term name) name   (Maybe (value term name) -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Domain term name value)

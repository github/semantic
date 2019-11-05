{-# LANGUAGE DeriveFunctor, DeriveGeneric, ExistentialQuantification, FlexibleContexts, LambdaCase, RankNTypes, StandaloneDeriving #-}
module Analysis.Analysis
( Analysis(..)
) where

import Analysis.Name
import Control.Effect.Carrier
import Data.Text (Text)
import GHC.Generics (Generic1)

-- | A record of functions necessary to perform analysis.
--
-- This is intended to be replaced with a selection of algebraic effects providing these interfaces and carriers providing reusable implementations.
data Analysis term address value m = Analysis
  { abstract  :: (term Name -> m value) -> Name -> term Name -> m value
  , apply     :: (term Name -> m value) -> value -> value -> m value
  , unit      :: m value
  , bool      :: Bool -> m value
  , asBool    :: value -> m Bool
  , string    :: Text -> m value
  , asString  :: value -> m Text
  , record    :: [(Name, value)] -> m value
  , (...)     :: address -> Name -> m (Maybe address)
  }


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
instance Effect   (Domain term name value)

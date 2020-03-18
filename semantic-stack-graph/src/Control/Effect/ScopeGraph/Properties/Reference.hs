{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | The 'Declaration' record type is used by the 'Control.Effect.Sketch' module to keep
-- track of the parameters that need to be passed when establishing a new reference.
-- It is currently unused, but will possess more fields in the future as scope graph
-- functionality is enhanced.
module Control.Effect.ScopeGraph.Properties.Reference
  ( Reference (..)
  ) where

import Control.Lens
import Data.ScopeGraph as ScopeGraph (Kind, Relation)
import GHC.Generics (Generic)
import Prelude hiding (span)
import Source.Span

data Reference = Reference
  { kind     :: ScopeGraph.Kind
  , relation :: ScopeGraph.Relation
  , span     :: Span
  } deriving (Generic, Show)

instance HasSpan Reference where
  span_ = lens span (\r s -> r { span = s })
  {-# INLINE span_ #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | The 'Declaration' record type is used by the 'Control.Effect.Sketch' module to keep
-- track of the parameters that need to be passed when establishing a new declaration.
-- That is to say, it is a record type primarily used for its selector names.
module Control.Effect.ScopeGraph.Properties.Declaration
  ( Declaration (..)
  ) where

import Analysis.Name (Name)
import Data.Generics.Product (field)
import Data.ScopeGraph as ScopeGraph (Kind, Relation)
import GHC.Generics (Generic)
import Source.Span

data Declaration = Declaration
  { kind            :: ScopeGraph.Kind
  , relation        :: ScopeGraph.Relation
  , associatedScope :: Maybe Name
  , span            :: Span
  } deriving Generic

instance HasSpan Declaration where span_ = field @"span"

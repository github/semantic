{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | The 'Function' record type is used by the 'Control.Effect.Sketch' module to keep
-- track of the parameters that need to be passed when establishing a new declaration.
-- That is to say, it is a record type primarily used for its selector names.
module Control.Effect.ScopeGraph.Properties.Function
  ( Function (..)
  ) where

import           Data.Generics.Product (field)
import qualified Data.ScopeGraph as ScopeGraph (Kind)
import           GHC.Generics (Generic)
import           Source.Span

data Function = Function
  { kind :: ScopeGraph.Kind
  , span :: Span
  } deriving Generic

instance HasSpan Function where span_ = field @"span"

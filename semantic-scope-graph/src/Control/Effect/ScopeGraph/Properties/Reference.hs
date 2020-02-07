-- | The 'Declaration' record type is used by the 'Control.Effect.Sketch' module to keep
-- track of the parameters that need to be passed when establishing a new reference.
-- It is currently unused, but will possess more fields in the future as scope graph
-- functionality is enhanced.
module Control.Effect.ScopeGraph.Properties.Reference
  ( Reference (..)
  ) where

data Reference = Reference

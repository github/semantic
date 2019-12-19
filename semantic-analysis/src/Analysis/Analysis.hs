{-# LANGUAGE KindSignatures #-}
module Analysis.Analysis
( Analysis(..)
) where

import Analysis.Name

-- | A record of functions necessary to perform analysis.
--
-- This is intended to be replaced with a selection of algebraic effects providing these interfaces and carriers providing reusable implementations.
data Analysis (term :: * -> *) address value m = Analysis
  { record   :: [(Name, value)] -> m value
  , (...)    :: address -> Name -> m (Maybe address)
  }

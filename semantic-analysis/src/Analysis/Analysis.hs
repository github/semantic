module Analysis.Analysis
( Analysis(..)
) where

import Analysis.Name
import Data.Text (Text)

-- | A record of functions necessary to perform analysis.
--
-- This is intended to be replaced with a selection of algebraic effects providing these interfaces and carriers providing reusable implementations.
data Analysis term address value m = Analysis
  { abstract :: (term Name -> m value) -> Name -> term Name -> m value
  , apply    :: (term Name -> m value) -> value -> value -> m value
  , string   :: Text -> m value
  , asString :: value -> m Text
  , record   :: [(Name, value)] -> m value
  , (...)    :: address -> Name -> m (Maybe address)
  }

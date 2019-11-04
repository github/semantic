module Analysis.Domain
( Domain(..)
) where

import Data.Text (Text)

data Domain
  = Unit
  | Bool Bool
  | String Text

module Analysis.Domain
( Domain(..)
) where

import Data.Text (Text)

data Domain name a
  = Unit
  | Bool Bool
  | String Text
  | Record [(name, a)]
  | Lam name a
  deriving (Eq, Ord, Show)

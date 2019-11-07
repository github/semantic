module Analysis.Intro
( Intro(..)
) where

import Data.Text (Text)

data Intro
  = Unit
  | Bool Bool
  | String Text
  deriving (Eq, Ord, Show)

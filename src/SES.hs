{-# LANGUAGE Strict #-}
module SES
( Comparable
, Myers.ses
) where

import Prologue
import qualified SES.Myers as Myers

-- | Edit constructor for two terms, if comparable. Otherwise returns Nothing.
type Comparable term = term -> term -> Bool

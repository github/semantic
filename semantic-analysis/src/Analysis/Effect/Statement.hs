{-# LANGUAGE GADTs #-}
module Analysis.Effect.Statement
( -- * Statement effect
  Statement(..)
) where

import Data.Kind as K

-- Statement effect

data Statement (m :: K.Type -> K.Type) k where

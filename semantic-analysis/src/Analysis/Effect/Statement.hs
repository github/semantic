{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Analysis.Effect.Statement
( -- * Statement effect
  simport
, Statement(..)
) where

import Control.Algebra
import Data.Kind as K
import Data.List.NonEmpty (NonEmpty)
import Data.Text

-- Statement effect

simport :: Has Statement sig m => NonEmpty Text -> m ()
simport ns = send (Import ns)

data Statement (m :: K.Type -> K.Type) k where
  Import :: NonEmpty Text -> Statement m ()

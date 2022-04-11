{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{- |
The @'Statement'@ effect is designed to provide instrumentation for source-level interactions we need visibility into which are nevertheless not (currently) modelled by expressions: e.g. statements, declarations, certain directives, etc.

Currently this is limited to imports, where the value-level semantics are (for many languages) essentially the unit value, but where the effect of bringing an environment and entire subset of the store into scope are essential to track for modular interpretation.
-}
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

{-# LANGUAGE DefaultSignatures, KindSignatures, TypeFamilies #-}
module Control.Abstract.Analysis
( MonadAnalysis(..)
, TermFor
, ValueFor
, module X
, Subterm(..)
, SubtermAlgebra
) where

import Control.Effect as X
import Control.Newtype1 as X
import Control.Monad.Effect.Fail as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.State as X
import Prologue

type family TermFor (m :: * -> *)
type family ValueFor (m :: * -> *)

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class Monad m => MonadAnalysis m where
  -- | Analyze a term using the semantics of the current analysis. This should generally only be called by definitions of 'evaluateTerm' and 'analyzeTerm' in this or other instances.
  analyzeTerm :: SubtermAlgebra (Base (TermFor m)) (TermFor m) (m (ValueFor m))

  -- | Evaluate a term to a value using the semantics of the current analysis.
  --
  --   This should always be called instead of explicitly folding either 'eval' or 'analyzeTerm' over subterms, except in 'MonadAnalysis' instances themselves.
  evaluateTerm :: TermFor m -> m (ValueFor m)
  default evaluateTerm :: Recursive (TermFor m) => TermFor m -> m (ValueFor m)
  evaluateTerm = foldSubterms analyzeTerm

delegateAnalyzeTerm :: (Functor (Base (TermFor m)), Newtype1 m, MonadAnalysis (O1 m), TermFor m ~ TermFor (O1 m), ValueFor m ~ ValueFor (O1 m)) => SubtermAlgebra (Base (TermFor m)) (TermFor m) (m (ValueFor m))
delegateAnalyzeTerm term = pack1 (analyzeTerm (second unpack1 <$> term))

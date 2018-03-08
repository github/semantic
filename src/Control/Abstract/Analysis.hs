{-# LANGUAGE DefaultSignatures, KindSignatures, TypeFamilies #-}
module Control.Abstract.Analysis
( MonadAnalysis(..)
, delegateAnalyzeTerm
, module X
, Subterm(..)
, SubtermAlgebra
) where

import Control.Abstract.Evaluator as X
import Control.Effect as X
import Control.Monad.Effect.Fail as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.State as X
import Data.Coerce
import Prologue

-- | A 'Monad' in which one can evaluate some specific term type to some specific value type.
--
--   This typeclass is left intentionally unconstrained to avoid circular dependencies between it and other typeclasses.
class MonadEvaluator m => MonadAnalysis m where
  -- | Analyze a term using the semantics of the current analysis. This should generally only be called by definitions of 'evaluateTerm' and 'analyzeTerm' in this or other instances.
  analyzeTerm :: SubtermAlgebra (Base (TermFor m)) (TermFor m) (m (ValueFor m))

  -- | Evaluate a term to a value using the semantics of the current analysis.
  --
  --   This should always be called instead of explicitly folding either 'eval' or 'analyzeTerm' over subterms, except in 'MonadAnalysis' instances themselves.
  evaluateTerm :: TermFor m -> m (ValueFor m)
  default evaluateTerm :: Recursive (TermFor m) => TermFor m -> m (ValueFor m)
  evaluateTerm = foldSubterms analyzeTerm

delegateAnalyzeTerm :: ( TermFor (t m) ~ TermFor m
                       , ValueFor (t m) ~ ValueFor m
                       , Functor (Base (TermFor (t m)))
                       , MonadAnalysis m
                       , Coercible (t m (ValueFor m)) (m (ValueFor m))
                       , Coercible (m (ValueFor m)) (t m (ValueFor m))
                       )
                    => SubtermAlgebra (Base (TermFor (t m))) (TermFor (t m)) (t m (ValueFor m))
delegateAnalyzeTerm term = pack1 (analyzeTerm (second unpack1 <$> term))
  where pack1 = coerce
        unpack1 :: Coercible (t m (ValueFor m)) (m (ValueFor m)) => t m (ValueFor m) -> m (ValueFor m)
        unpack1 = coerce

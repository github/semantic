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
import Control.Monad.Effect.Fail as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.State as X
import Data.Coerce
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


class Newtype1 n where
  type O1 n :: * -> *

  pack1 :: O1 n a -> n a
  default pack1 :: (Generic1 n, GNewtype1 (Rep1 n), O1 n ~ GO1 (Rep1 n)) => O1 n a -> n a
  pack1 = to1 . gpack1

  unpack1 :: n a -> O1 n a
  default unpack1 :: (Generic1 n, GNewtype1 (Rep1 n), O1 n ~ GO1 (Rep1 n)) => n a -> O1 n a
  unpack1 = gunpack1 . from1

class GNewtype1 n where
  type GO1 n :: * -> *

  gpack1 :: GO1 n a -> n a
  gunpack1 :: n a -> GO1 n a

instance GNewtype1 (D1 d (C1 c (S1 s (Rec1 a)))) where
  type GO1 (D1 d (C1 c (S1 s (Rec1 a)))) = a
  gpack1 = coerce
  gunpack1 = coerce

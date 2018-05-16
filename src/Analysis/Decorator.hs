{-# LANGUAGE DataKinds, TypeOperators #-}
module Analysis.Decorator
( decoratorWithAlgebra
) where

import Data.Record
import Data.Term
import Prologue

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: Functor syntax
                     => RAlgebra (TermF syntax (Record fs)) (Term syntax (Record fs)) a -- ^ An R-algebra on terms.
                     -> Term syntax (Record fs)                                         -- ^ A term to decorate with values produced by the R-algebra.
                     -> Term syntax (Record (a ': fs))                                  -- ^ A term decorated with values produced by the R-algebra.
decoratorWithAlgebra alg = para $ \ c@(In a f) -> termIn (alg (fmap (second (rhead . termAnnotation)) c) :. a) (fmap snd f)

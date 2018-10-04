{-# LANGUAGE DataKinds, TypeOperators #-}
module Analysis.Decorator
( decoratorWithAlgebra
) where

import Data.Term
import Prologue

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: Functor syntax
                     => RAlgebra (TermF syntax a) (Term syntax a) b -- ^ An R-algebra on terms.
                     -> Term syntax a                               -- ^ A term to decorate with values produced by the R-algebra.
                     -> Term syntax b                               -- ^ A term decorated with values produced by the R-algebra.
decoratorWithAlgebra alg = para $ \ c@(In _ f) -> termIn (alg (fmap (second termAnnotation) c)) (fmap snd f)

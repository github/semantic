{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Analysis.Decorator
( decoratorWithAlgebra
) where

import Data.Term
import Prologue

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: (Functor (Syntax term), IsTerm term, Recursive (term a), Base (term a) ~ TermF (Syntax term) a)
                     => RAlgebra (TermF (Syntax term) a) (term a) b -- ^ An R-algebra on terms.
                     -> term a                                      -- ^ A term to decorate with values produced by the R-algebra.
                     -> term b                                      -- ^ A term decorated with values produced by the R-algebra.
decoratorWithAlgebra alg = para $ \ c@(In _ f) -> termIn (alg (fmap (second termAnnotation) c)) (fmap snd f)

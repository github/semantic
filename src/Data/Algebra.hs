{-# LANGUAGE RankNTypes #-}
module Data.Algebra
( FAlgebra
, RAlgebra
, OpenFAlgebra
, OpenRAlgebra
, fToR
, fToOpenR
, rToOpenR
, openFToOpenR
) where

import Data.Functor.Foldable (Base)

-- | An F-algebra on some 'Recursive' type @t@.
--
--   An F-algebra is an evaluator for a functor (@Base t@) populated with the results (@a@s) of evaluating each child of the functor to a result (@a@), applied at each node starting from the leaves and working towards the root. @a@ is called the carrier type of the algebra because it carries the (results of the) functions used to compute the functor’s structure and enforce the laws of that algebra.
type FAlgebra t a = Base t a -> a

-- | An R-algebra on some 'Recursive' type @t@.
--
--   An R-algebra is an evaluator for a functor (@Base t@) populated with pairs of the children (@t@) and the results (@a@s) of evaluating each child of the functor to a result (@a@), applied at each node starting from the leaves and working towards the root.
--
--   See also 'FAlgebra'.
type RAlgebra t a = Base t (t, a) -> a

-- | An open-recursive F-algebra on some 'Recursive' type @t@.
--
--   The recursion is “open” because instead of being applied from the leaves towards the root like a regular 'FAlgebra', the functor (@Base t@) is populated with the original values (@b@), and each is evaluated via the continuation (@(b -> a)@).
--
--   See also 'FAlgebra'.
type OpenFAlgebra t a = forall b . (b -> a) -> Base t b -> a

-- | An open-recursive R-algebra on some 'Recursive' type @t@.
--
--   See also 'RAlgebra' & 'OpenFAlgebra'.
type OpenRAlgebra t a = forall b . (b -> (t, a)) -> Base t b -> a

-- | Promote an 'FAlgebra' into an 'RAlgebra' (by dropping the original parameter).
fToR :: Functor (Base t) => FAlgebra t a -> RAlgebra t a
fToR f = f . fmap snd

-- | Promote an 'FAlgebra' into an 'OpenRAlgebra' (by 'fmap'ing the action over the structure and dropping the original parameter).
fToOpenR :: Functor (Base t) => FAlgebra t a -> OpenRAlgebra t a
fToOpenR alg f = alg . fmap (snd . f)

-- | Promote an 'RAlgebra' into an 'OpenRAlgebra' (by 'fmap'ing the action over the structure).
rToOpenR :: Functor (Base t) => RAlgebra t a -> OpenRAlgebra t a
rToOpenR alg f = alg . fmap f

-- | Promote an 'OpenFAlgebra' into an 'OpenRAlgebra' (by dropping the original parameter).
openFToOpenR :: OpenFAlgebra t a -> OpenRAlgebra t a
openFToOpenR alg = alg . fmap snd

{-# LANGUAGE MultiParamTypeClasses, NoStrictData #-}
module Data.Amb where

import Control.Monad.Error.Class
import Data.Bifunctor
import Data.List.NonEmpty
import Data.Semigroup

-- | The ambiguity monad, the result of nondeterministic choice.
--
--   Values are either the empty set (with an error value indicating why), or a 'NonEmpty' list of ambiguous results.
data Amb l r
  = None l            -- ^ No results; an error.
  | Some (NonEmpty r) -- ^ One or more results.
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | Disambiguate an 'Amb' by case analysis.
--
--   If the value is @'None' l@, apply the first function to @l@; if it is @'Some' rs@, apply the second function to @rs@.
disamb :: (l -> a) -> (NonEmpty r -> a) -> Amb l r -> a
disamb f _ (None l) = f l
disamb _ g (Some r) = g r


instance Bifunctor Amb where
  bimap f _ (None l) = None (f l)
  bimap _ g (Some r) = Some (fmap g r)

instance Semigroup (Amb l r) where
  l <> None _ = l
  None _ <> r = r
  Some as <> Some bs = Some (as <> bs)

instance Applicative (Amb l) where
  pure a = Some (a :| [])
  None l <*> _ = None l
  _ <*> None r = None r
  Some fs <*> Some as = Some (fs <*> as)

instance Monad (Amb l) where
  return = pure
  None a >>= _ = None a
  Some as >>= f = sconcat (f <$> as)

instance MonadError l (Amb l) where
  throwError = None
  None a  `catchError` f = f a
  Some as `catchError` _ = Some as

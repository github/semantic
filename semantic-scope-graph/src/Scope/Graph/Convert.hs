{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Scope.Graph.Convert
  ( ToScopeGraph (..),
    Result (..),
    todo,
    complete,
  )
where

import Control.Effect.StackGraph
import Data.List.NonEmpty
import Data.Typeable
import Source.Loc

class Typeable t => ToScopeGraph t where
  type FocalPoint (t :: * -> *) (a :: *)
  scopeGraph ::
    ( StackGraphEff sig m
    ) =>
    t Loc ->
    m (Result (FocalPoint t Loc))

data Result a
  = Complete a
  | Todo (NonEmpty String)
  deriving (Eq, Show, Ord)

instance Functor Result where
  fmap f (Complete x) = Complete (f x)
  fmap _ (Todo msg) = Todo msg

instance Semigroup a => Semigroup (Result a) where
  Complete a <> Complete b = Complete (a <> b)
  Todo a <> Todo b = Todo (a <> b)
  a <> Complete _ = a
  Complete _ <> b = b

instance Monoid a => Monoid (Result a) where mempty = Complete mempty

instance Applicative Result where
  Complete a <*> Complete b = Complete (a b)
  Todo a <*> Todo b = Todo (a <> b)
  (Todo a) <*> _ = Todo a
  _ <*> (Todo b) = Todo b
  pure = Complete

todo :: Show a => a -> b
todo = error . show

complete :: (Monoid b, Applicative m) => m (Result b)
complete = pure (Complete mempty)

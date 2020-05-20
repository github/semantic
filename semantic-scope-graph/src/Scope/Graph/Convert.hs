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

import Control.Effect.ScopeGraph
import Data.List.NonEmpty
import Data.Typeable
import Source.Loc

class Typeable t => ToScopeGraph t where
  type FocalPoint (t :: * -> *)
  scopeGraph ::
    ( ScopeGraphEff sig m
    ) =>
    t Loc ->
    m (Result (FocalPoint t))

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

todo :: (Show a, Applicative m) => a -> m (Result b)
todo = pure . Todo . pure . show

complete :: (Monoid b, Applicative m) => m (Result b)
complete = pure (Complete mempty)

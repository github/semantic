{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Stack.Graph as Stack

class Typeable t => ToScopeGraph t where
  scopeGraph ::
    ( StackGraphEff sig m
    ) =>
    t Loc ->
    m Result

data Result
  = Complete
  | ValueNode Stack.Node
  | ReturnNodes [Stack.Node]
  | ParamNode Stack.Node
  | Todo (NonEmpty String)
  deriving (Eq, Show, Ord)

instance Semigroup Result where
  Complete <> Complete = Complete
  Todo a <> Todo b = Todo (a <> b)
  a <> Complete = a
  Complete <> a = a

instance Monoid Result where mempty = Complete

todo :: (Show a, Applicative m) => a -> m Result
todo = pure . Todo . pure . show

complete :: Applicative m => m Result
complete = pure Complete

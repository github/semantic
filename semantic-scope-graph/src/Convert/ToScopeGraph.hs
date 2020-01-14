{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Convert.ToScopeGraph
  ( ToScopeGraph (..)
  , Result (..)
  , onChildren
  , onField
  ) where

import Control.Effect.Sketch
import Data.Foldable
import Data.List.NonEmpty
import Data.Name (Name)
import Data.Typeable
import GHC.Generics
import GHC.Records
import GHC.TypeLits
import Source.Loc

class Typeable t => ToScopeGraph t where
  scopeGraph ::
    ( Has (Sketch Name) sig m
    )
    => t Loc
    -> m Result

data Result
  = Complete
  | Todo (NonEmpty String)
    deriving (Eq, Show, Ord)

instance Semigroup Result where
  Complete <> Complete = Complete
  Todo a <> Todo b     = Todo (a <> b)
  a <> Complete        = a
  Complete <> a        = a

instance Monoid Result where mempty = Complete

instance (ToScopeGraph l, ToScopeGraph r) => ToScopeGraph (l :+: r) where
  scopeGraph (L1 l) = scopeGraph l
  scopeGraph (R1 r) = scopeGraph r

onField ::
  forall (field :: Symbol) syn sig m r .
  ( Has (Sketch Name) sig m
  , HasField field (r Loc) (syn Loc)
  , ToScopeGraph syn
  )
  => r Loc
  -> m Result
onField
  = scopeGraph @syn
  . getField @field

onChildren ::
  ( Traversable t
  , ToScopeGraph syn
  , Has (Sketch Name) sig m
  , HasField "extraChildren" (r Loc) (t (syn Loc))
  )
  => r Loc
  -> m Result
onChildren
  = fmap fold
  . traverse scopeGraph
  . getField @"extraChildren"

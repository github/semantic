{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
module Convert.ToScopeGraph
  ( ToScopeGraph (..)
  , onChildren
  , onField
  ) where

import Control.Effect.Sketch
import Data.Foldable
import Data.ScopeGraph
import GHC.Generics
import GHC.Records
import Source.Loc

class ToScopeGraph t where
  scopeGraph ::
    ( Has (Sketch Info) sig m
    )
    => t Loc
    -> m (ScopeGraph Info)

instance (ToScopeGraph l, ToScopeGraph r) => ToScopeGraph (l :+: r) where
  scopeGraph (L1 l) = scopeGraph l
  scopeGraph (R1 r) = scopeGraph r

onField ::
  forall field syn sig m r .
  ( Has (Sketch Info) sig m
  , HasField field (r Loc) (syn Loc)
  , ToScopeGraph syn
  )
  => r Loc
  -> m (ScopeGraph Info)
onField
  = scopeGraph @syn
  . getField @field

onChildren ::
  ( Traversable t
  , ToScopeGraph syn
  , Has (Sketch Info) sig m
  , HasField "extraChildren" (r Loc) (t (syn Loc))
  )
  => r Loc
  -> m (ScopeGraph Info)
onChildren
  = fmap fold
  . traverse scopeGraph
  . getField @"extraChildren"

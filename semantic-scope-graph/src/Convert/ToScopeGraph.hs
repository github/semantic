{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Convert.ToScopeGraph
  ( ToScopeGraph (..)
  , onChildren
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

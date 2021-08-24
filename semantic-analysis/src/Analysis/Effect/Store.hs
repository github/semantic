{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Analysis.Effect.Store
( -- * Store effect
  alloc
, (.=)
, fetch
, Store(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Analysis.Name
import Control.Effect.Labelled
import Data.Kind as K

-- Store effect

alloc :: HasLabelled Store (Store addr val) sig m => Name -> m addr
alloc = sendLabelled @Store . Alloc

(.=) :: HasLabelled Store (Store addr val) sig m => addr -> val -> m ()
addr .= val = sendLabelled @Store $ Assign addr val

infix 2 .=

fetch :: HasLabelled Store (Store addr val) sig m => addr -> m val
fetch = sendLabelled @Store . Fetch

data Store addr val (m :: K.Type -> K.Type) k where
  Alloc :: Name -> Store addr val m addr
  Assign :: addr -> val -> Store addr val m ()
  Fetch :: addr -> Store addr val m val

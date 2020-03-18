{-# LANGUAGE OverloadedStrings #-}
module Scope.Reference
  ( ReferenceInfo (..)
  , Reference (..)
  ) where

import Analysis.Name
import Control.Lens (lens)
import Data.Module
import Data.Semilattice.Lower
import Scope.Types
import Source.Span

data ReferenceInfo = ReferenceInfo
  { refSpan   :: Span
  , refKind   :: Kind
  , refModule :: ModuleInfo
  } deriving (Eq, Show, Ord)

instance HasSpan ReferenceInfo where
  span_ = lens refSpan (\r s -> r { refSpan = s })
  {-# INLINE span_ #-}

newtype Reference = Reference { unReference :: Name }
  deriving (Eq, Ord, Show)

instance Lower Reference where
  lowerBound = Reference $ name ""

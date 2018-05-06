{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Data.Abstract.Located where

import Control.Abstract.Addressable
import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Package (PackageInfo)
import Prologue

data Located location = Located
  { location        :: location
  , locationPackage :: {-# UNPACK #-} !PackageInfo
  , locationModule  :: {-# UNPACK #-} !ModuleInfo
  }
  deriving (Eq, Ord, Show)

instance Location location => Location (Located location) where
  type Cell (Located location) = Cell location

instance ( Addressable location effects
         , Members '[ Reader ModuleInfo
                    , Reader PackageInfo
                    ] effects
         )
      => Addressable (Located location) effects where
  derefCell (Address (Located loc _ _)) = raise . lower . derefCell (Address loc)

  allocLoc name = raise (lower (Located <$> allocLoc name <*> currentPackage <*> currentModule))

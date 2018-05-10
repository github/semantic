{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Data.Abstract.Located where

import Control.Abstract
import Data.Abstract.Address
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Package (PackageInfo)

data Located location = Located
  { location        :: location
  , locationPackage :: {-# UNPACK #-} !PackageInfo
  , locationModule  :: !ModuleInfo
  }
  deriving (Eq, Ord, Show)

instance Location (Located location) where
  type Cell (Located location) = Cell location

instance ( Addressable location effects
         , Members '[ Reader ModuleInfo
                    , Reader PackageInfo
                    ] effects
         )
      => Addressable (Located location) effects where
  derefCell (Address (Located loc _ _)) = raiseEff . lowerEff . derefCell (Address loc)

  allocLoc name = raiseEff (lowerEff (Located <$> allocLoc name <*> currentPackage <*> currentModule))

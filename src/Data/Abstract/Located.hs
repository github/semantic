{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Data.Abstract.Located where

import Control.Abstract
import Data.Abstract.Address
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Package (PackageInfo)

data Located location (cell :: * -> *) = Located
  { location        :: location cell
  , locationPackage :: {-# UNPACK #-} !PackageInfo
  , locationModule  :: !ModuleInfo
  }
  deriving (Eq, Ord, Show)

instance Location (Located location cell) where
  type Cell (Located location cell) = Cell (location cell)

instance ( Addressable (location cell) effects
         , Members '[ Reader ModuleInfo
                    , Reader PackageInfo
                    ] effects
         )
      => Addressable (Located location cell) effects where
  derefCell (Address (Located loc _ _)) = raiseEff . lowerEff . derefCell (Address loc)

  allocLoc name = raiseEff (lowerEff (Located <$> allocLoc name <*> currentPackage <*> currentModule))

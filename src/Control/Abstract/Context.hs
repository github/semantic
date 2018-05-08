module Control.Abstract.Context
( ModuleInfo
, PackageInfo
, currentModule
, currentPackage
) where

import Control.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Module
import Data.Abstract.Package
import Prologue

-- | Get the currently evaluating 'ModuleInfo'.
currentModule :: (Effectful m, Member (Reader ModuleInfo) effects) => m effects ModuleInfo
currentModule = raise ask

-- | Get the currently evaluating 'PackageInfo'.
currentPackage :: (Effectful m, Member (Reader PackageInfo) effects) => m effects PackageInfo
currentPackage = raise ask

module Data.Abstract.Module
( Module(..)
, moduleForBlob
, ModulePath
, ModuleInfo(..)
, moduleInfoFromSrcLoc
, moduleInfoFromCallStack
) where

import Data.Blob
import GHC.Stack
import Prologue
import System.FilePath.Posix

data Module body = Module { moduleInfo :: ModuleInfo, moduleBody :: body }
  deriving (Eq, Foldable, Functor, Ord, Traversable)

instance Show body => Show (Module body) where
  showsPrec d Module{..} = showsBinaryWith showsPrec showsPrec "Module" d (modulePath moduleInfo) moduleBody


-- | Construct a 'Module' for a 'Blob' and @term@, relative to some root 'FilePath'.
moduleForBlob :: Maybe FilePath -- ^ The root directory relative to which the module will be resolved, if any.
              -> Blob           -- ^ The 'Blob' containing the module.
              -> term           -- ^ The @term@ representing the body of the module.
              -> Module term    -- ^ A 'Module' named appropriate for the 'Blob', holding the @term@, and constructed relative to the root 'FilePath', if any.
moduleForBlob rootDir Blob{..} = Module info
  where root = fromMaybe (takeDirectory blobPath) rootDir
        info = ModuleInfo (makeRelative root blobPath)


type ModulePath = FilePath

newtype ModuleInfo = ModuleInfo { modulePath :: ModulePath }
  deriving (Eq, Ord)

instance Show ModuleInfo where
  showsPrec d = showsUnaryWith showsPrec "ModuleInfo" d . modulePath

moduleInfoFromSrcLoc :: SrcLoc -> ModuleInfo
moduleInfoFromSrcLoc = ModuleInfo . srcLocModule

-- | Produce 'ModuleInfo' from the top location on the Haskell call stack (i.e. the file where the call to 'moduleInfoFromCallStack' was made).
moduleInfoFromCallStack :: HasCallStack => ModuleInfo
moduleInfoFromCallStack = maybe (ModuleInfo "?") (moduleInfoFromSrcLoc . snd) (listToMaybe (getCallStack callStack))

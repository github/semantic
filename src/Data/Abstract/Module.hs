{-# LANGUAGE DeriveTraversable, RecordWildCards #-}
module Data.Abstract.Module
( Module(..)
, moduleForBlob
, ModulePath
, ModuleInfo(..)
, moduleInfoFromSrcLoc
, moduleInfoFromCallStack
) where

import Data.Blob
import Data.Language
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
moduleForBlob rootDir b = Module info
  where root = fromMaybe (takeDirectory (blobPath b)) rootDir
        info = ModuleInfo (makeRelative root (blobPath b)) (blobLanguage b) (blobOid b)


type ModulePath = FilePath

data ModuleInfo = ModuleInfo { modulePath :: ModulePath, moduleLanguage :: Language, moduleOid :: Text }
  deriving (Eq, Ord)

instance Lower ModuleInfo where
  lowerBound = ModuleInfo mempty Unknown mempty

instance Show ModuleInfo where
  showsPrec d = showsUnaryWith showsPrec "ModuleInfo" d . modulePath

moduleInfoFromSrcLoc :: SrcLoc -> ModuleInfo
moduleInfoFromSrcLoc loc = ModuleInfo (srcLocModule loc) Unknown mempty

-- | Produce 'ModuleInfo' from the top location on the Haskell call stack (i.e. the file where the call to 'moduleInfoFromCallStack' was made).
moduleInfoFromCallStack :: HasCallStack => ModuleInfo
moduleInfoFromCallStack = maybe (ModuleInfo "?" Unknown mempty) (moduleInfoFromSrcLoc . snd) (listToMaybe (getCallStack callStack))

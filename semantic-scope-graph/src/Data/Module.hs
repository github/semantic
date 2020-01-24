{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Module
( Module(..)
, ModulePath
, ModuleInfo(..)
, moduleInfoFromSrcLoc
, moduleInfoFromCallStack
) where

import Data.Functor.Classes
import Data.Maybe
import Data.Semilattice.Lower
import Data.Serialize
import Data.Serialize.Text ()
import Data.Text (Text)
import GHC.Generics
import GHC.Stack

data Module body = Module { moduleInfo :: ModuleInfo, moduleBody :: body }
  deriving (Eq, Foldable, Functor, Ord, Traversable)

instance Show body => Show (Module body) where
  showsPrec d Module{..} = showsBinaryWith showsPrec showsPrec "Module" d (modulePath moduleInfo) moduleBody


type ModulePath = FilePath

data ModuleInfo = ModuleInfo { modulePath :: ModulePath, moduleLanguage :: Text, moduleOid :: Text }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass Serialize

instance Lower ModuleInfo where
  lowerBound = ModuleInfo mempty "Unknown" mempty

instance Show ModuleInfo where
  showsPrec d = showsUnaryWith showsPrec "ModuleInfo" d . modulePath

moduleInfoFromSrcLoc :: SrcLoc -> ModuleInfo
moduleInfoFromSrcLoc loc = ModuleInfo (srcLocModule loc) "Unknown" mempty

-- | Produce 'ModuleInfo' from the top location on the Haskell call stack (i.e. the file where the call to 'moduleInfoFromCallStack' was made).
moduleInfoFromCallStack :: HasCallStack => ModuleInfo
moduleInfoFromCallStack = maybe (ModuleInfo "?" "Unknown" mempty) (moduleInfoFromSrcLoc . snd) (listToMaybe (getCallStack callStack))

module Control.Abstract.Context
( ModuleInfo
, currentModule
, withCurrentModule
, PackageInfo
, currentPackage
, withCurrentPackage
, Span
, currentSpan
, withCurrentSpan
, withCurrentCallStack
) where

import Control.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Module
import Data.Abstract.Package
import Data.Span
import GHC.Stack
import Prologue

-- | Get the currently evaluating 'ModuleInfo'.
currentModule :: (Effectful m, Member (Reader ModuleInfo) effects) => m effects ModuleInfo
currentModule = raise ask

-- | Run an action with a locally-replaced 'ModuleInfo'.
withCurrentModule :: (Effectful m, Member (Reader ModuleInfo) effects) => ModuleInfo -> m effects a -> m effects a
withCurrentModule = raiseHandler . local . const

-- | Get the currently evaluating 'PackageInfo'.
currentPackage :: (Effectful m, Member (Reader PackageInfo) effects) => m effects PackageInfo
currentPackage = raise ask

-- | Run an action with a locally-replaced 'PackageInfo'.
withCurrentPackage :: (Effectful m, Member (Reader PackageInfo) effects) => PackageInfo -> m effects a -> m effects a
withCurrentPackage = raiseHandler . local . const

-- | Get the 'Span' of the currently-evaluating term (if any).
currentSpan :: (Effectful m, Member (Reader Span) effects) => m effects Span
currentSpan = raise ask

-- | Run an action with a locally-replaced 'Span'.
withCurrentSpan :: (Effectful m, Member (Reader Span) effects) => Span -> m effects a -> m effects a
withCurrentSpan = raiseHandler . local . const


-- | Run an action with locally-replaced 'ModuleInfo' & 'Span' derived from the passed 'SrcLoc'.
withCurrentSrcLoc :: (Effectful m, Members '[Reader ModuleInfo, Reader Span] effects) => SrcLoc -> m effects a -> m effects a
withCurrentSrcLoc loc = withCurrentModule (moduleInfoFromSrcLoc loc) . withCurrentSpan (spanFromSrcLoc loc)

-- | Run ana ction with locally replaced 'ModuleInfo' & 'Span' derived from the Haskell call stack.
--
--   This is suitable for contextualizing builtins & other functionality intended for use from client code but defined in Haskell source.
withCurrentCallStack :: (Effectful m, Members '[Reader ModuleInfo, Reader Span] effects) => CallStack -> m effects a -> m effects a
withCurrentCallStack = maybe id (withCurrentSrcLoc . snd) . listToMaybe . getCallStack

{-# LANGUAGE FlexibleContexts #-}
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
, modifyChildSpan
, withCurrentCallStack
) where

import Control.Effect.Reader
import Control.Effect.State
import Data.Abstract.Module
import Data.Abstract.Package
import Data.Maybe
import GHC.Stack
import Source.Span

-- | Get the currently evaluating 'ModuleInfo'.
currentModule :: (Has (Reader ModuleInfo) sig m) => m ModuleInfo
currentModule = ask

-- | Run an action with a locally-replaced 'ModuleInfo'.
withCurrentModule :: Has (Reader ModuleInfo) sig m => ModuleInfo -> m a -> m a
withCurrentModule = local . const

-- | Get the currently evaluating 'PackageInfo'.
currentPackage :: Has (Reader PackageInfo) sig m => m PackageInfo
currentPackage = ask

-- | Run an action with a locally-replaced 'PackageInfo'.
withCurrentPackage :: Has (Reader PackageInfo) sig m => PackageInfo -> m a -> m a
withCurrentPackage = local . const

-- | Get the 'Span' of the currently-evaluating term (if any).
currentSpan :: Has (Reader Span) sig m => m Span
currentSpan = ask

-- | Run an action with a locally-replaced 'Span'.
withCurrentSpan :: Has (Reader Span) sig m => Span -> m a -> m a
withCurrentSpan = local . const

modifyChildSpan :: Has (State Span) sig m => Span -> m a -> m a
modifyChildSpan span m = m <* put span

-- | Run an action with locally-replaced 'ModuleInfo' & 'Span' derived from the passed 'SrcLoc'.
withCurrentSrcLoc :: (Has (Reader ModuleInfo) sig m, Has (Reader Span) sig m) => SrcLoc -> m a -> m a
withCurrentSrcLoc loc = withCurrentModule (moduleInfoFromSrcLoc loc) . withCurrentSpan (spanFromSrcLoc loc)

-- | Run an action with locally replaced 'ModuleInfo' & 'Span' derived from the Haskell call stack.
--
--   This is suitable for contextualizing builtins & other functionality intended for use from client code but defined in Haskell source.
withCurrentCallStack :: (Has (Reader ModuleInfo) sig m, Has (Reader Span) sig m) => CallStack -> m a -> m a
withCurrentCallStack = maybe id (withCurrentSrcLoc . snd) . listToMaybe . getCallStack

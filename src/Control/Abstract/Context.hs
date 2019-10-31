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
import GHC.Stack
import Prologue
import Source.Span

-- | Get the currently evaluating 'ModuleInfo'.
currentModule :: (Member (Reader ModuleInfo) sig, Carrier sig m) => m ModuleInfo
currentModule = ask

-- | Run an action with a locally-replaced 'ModuleInfo'.
withCurrentModule :: (Member (Reader ModuleInfo) sig, Carrier sig m) => ModuleInfo -> m a -> m a
withCurrentModule = local . const

-- | Get the currently evaluating 'PackageInfo'.
currentPackage :: (Member (Reader PackageInfo) sig, Carrier sig m) => m PackageInfo
currentPackage = ask

-- | Run an action with a locally-replaced 'PackageInfo'.
withCurrentPackage :: (Member (Reader PackageInfo) sig, Carrier sig m) => PackageInfo -> m a -> m a
withCurrentPackage = local . const

-- | Get the 'Span' of the currently-evaluating term (if any).
currentSpan :: (Member (Reader Span) sig, Carrier sig m) => m Span
currentSpan = ask

-- | Run an action with a locally-replaced 'Span'.
withCurrentSpan :: (Member (Reader Span) sig, Carrier sig m) => Span -> m a -> m a
withCurrentSpan = local . const

modifyChildSpan :: (Member (State Span) sig, Carrier sig m) => Span -> m a -> m a
modifyChildSpan span m = m <* put span

-- | Run an action with locally-replaced 'ModuleInfo' & 'Span' derived from the passed 'SrcLoc'.
withCurrentSrcLoc :: (Member (Reader ModuleInfo) sig, Member (Reader Span) sig, Carrier sig m) => SrcLoc -> m a -> m a
withCurrentSrcLoc loc = withCurrentModule (moduleInfoFromSrcLoc loc) . withCurrentSpan (spanFromSrcLoc loc)

-- | Run an action with locally replaced 'ModuleInfo' & 'Span' derived from the Haskell call stack.
--
--   This is suitable for contextualizing builtins & other functionality intended for use from client code but defined in Haskell source.
withCurrentCallStack :: (Member (Reader ModuleInfo) sig, Member (Reader Span) sig, Carrier sig m) => CallStack -> m a -> m a
withCurrentCallStack = maybe id (withCurrentSrcLoc . snd) . listToMaybe . getCallStack

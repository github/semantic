{-# LANGUAGE RecordWildCards #-}
module Core.Loc
( here
, stackLoc
) where

import GHC.Stack
import Source.Span
import qualified System.Path as Path

here :: HasCallStack => Maybe (Path.AbsRelFile, Span)
here = stackLoc callStack

stackLoc :: CallStack -> Maybe (Path.AbsRelFile, Span)
stackLoc cs = case getCallStack cs of
  (_, srcLoc):_ -> Just (fromGHCSrcLoc srcLoc)
  _             -> Nothing

fromGHCSrcLoc :: SrcLoc -> (Path.AbsRelFile, Span)
fromGHCSrcLoc SrcLoc{..} = (Path.absRel srcLocFile, Span (Pos srcLocStartLine srcLocStartCol) (Pos srcLocEndLine srcLocEndCol))

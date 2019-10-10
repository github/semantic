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
  (_, srcLoc):_ -> Just (fromSrcLoc srcLoc)
  _             -> Nothing

fromSrcLoc :: SrcLoc -> (Path.AbsRelFile, Span)
fromSrcLoc SrcLoc{..} = (Path.absRel srcLocFile, Span (Pos srcLocStartLine srcLocStartCol) (Pos srcLocEndLine srcLocEndCol))

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Data.Loc
( Loc(..)
, interactive
, here
, stackLoc
) where

import Data.Text (Text, pack)
import GHC.Stack
import Source.Span

data Loc = Loc
  { locPath :: !Text
  , locSpan :: {-# UNPACK #-} !Span
  }
  deriving (Eq, Ord, Show)

interactive :: Loc
interactive = Loc "<interactive>" (Span (Pos 1 1) (Pos 1 1))


here :: HasCallStack => Maybe Loc
here = stackLoc callStack

stackLoc :: CallStack -> Maybe Loc
stackLoc cs = case getCallStack cs of
  (_, srcLoc):_ -> Just (fromGHCSrcLoc srcLoc)
  _             -> Nothing

fromGHCSrcLoc :: SrcLoc -> Loc
fromGHCSrcLoc SrcLoc{..} = Loc (pack srcLocFile) (Span (Pos srcLocStartLine srcLocStartCol) (Pos srcLocEndLine srcLocEndCol))

{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, TypeOperators, UndecidableInstances #-}
module Data.Loc
( Loc(..)
, interactive
, Span(..)
, emptySpan
, Pos(..)
, here
, stackLoc
, FailWithLocC(..)
, runFailWithLoc
) where

import Control.Applicative
import Control.Carrier
import Control.Carrier.Error.Either
import Control.Effect.Fail
import Control.Effect.Reader
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Pretty (..))
import GHC.Stack
import Prelude hiding (fail)

data Loc = Loc
  { locPath :: !Text
  , locSpan :: {-# UNPACK #-} !Span
  }
  deriving (Eq, Ord, Show)

interactive :: Loc
interactive = Loc "<interactive>" emptySpan

data Span = Span
  { spanStart :: {-# UNPACK #-} !Pos
  , spanEnd   :: {-# UNPACK #-} !Pos
  }
  deriving (Eq, Ord, Show)

instance Pretty Span where
  pretty (Span s e) = pretty s <> "-" <> pretty e

emptySpan :: Span
emptySpan = Span (Pos 1 1) (Pos 1 1)

data Pos = Pos
  { posLine :: {-# UNPACK #-} !Int
  , posCol  :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

instance Pretty Pos where
  pretty (Pos l c) = pretty l <> ":" <> pretty c


here :: HasCallStack => Maybe Loc
here = stackLoc callStack

stackLoc :: CallStack -> Maybe Loc
stackLoc cs = case getCallStack cs of
  (_, srcLoc):_ -> Just (fromGHCSrcLoc srcLoc)
  _             -> Nothing

fromGHCSrcLoc :: SrcLoc -> Loc
fromGHCSrcLoc SrcLoc{..} = Loc (pack srcLocFile) (Span (Pos srcLocStartLine srcLocStartCol) (Pos srcLocEndLine srcLocEndCol))


runFailWithLoc :: FailWithLocC m a -> m (Either (Loc, String) a)
runFailWithLoc = runError . runFailWithLocC

newtype FailWithLocC m a = FailWithLocC { runFailWithLocC :: ErrorC (Loc, String) m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Effect sig, Has (Reader Loc) sig m) => MonadFail (FailWithLocC m) where
  fail s = do
    loc <- ask
    FailWithLocC (throwError (loc :: Loc, s))

instance (Effect sig, Has (Reader Loc) sig m) => Carrier (Fail :+: sig) (FailWithLocC m) where
  eff (L (Fail s)) = fail s
  eff (R other)    = FailWithLocC (eff (R (handleCoercible other)))

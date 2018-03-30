{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Origin where

import Control.Effect
import Control.Monad.Effect.Reader
import Data.Abstract.Module
import Data.Range
import Data.Record
import Data.Span
import Data.Term
import Prologue

-- TODO: Upstream dependencies
data Origin
  = Unknown
  | Local !ModuleName !FilePath !Range !Span
  deriving (Eq, Ord, Show)


class HasOrigin f where
  originFor :: [Module a] -> f b -> Origin

instance (HasField fields Range, HasField fields Span) => HasOrigin (TermF syntax (Record fields)) where
  originFor []    _          = Unknown
  originFor (m:_) (In ann _) = Local (moduleName m) (modulePath m) (getField ann) (getField ann)


class Monad m => MonadOrigin m where
  askOrigin :: m Origin

instance ( Effectful m
         , Member (Reader Origin) effects
         , Monad (m effects)
         )
      => MonadOrigin (m effects) where
  askOrigin = raise ask


instance Semigroup Origin where
  a       <> Unknown = a
  _       <> b       = b

instance Monoid Origin where
  mempty = Unknown
  mappend = (<>)

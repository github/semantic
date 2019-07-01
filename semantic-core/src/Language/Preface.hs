{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
-- | This module defines a Preface effect that Core backends for
-- languages interpret to build Core expressions that provide
-- builtin classes and functions. It is based loosely on
-- 'Language.Haskell.TH.Lib'.

module Language.Preface
  ( Preface (..)
  , def
  , klass
  , method
  , open
  , metaclass
  , inherit
  , frame
  , call
  , new
  , var
  ) where

import Control.Effect.Carrier
import Control.Effect.Sum
import GHC.Stack
import Unsafe.Coerce

data Preface m k
  = forall a. Def CallStack String (m a) (a -> k)
  | forall a. Klass CallStack String (m a) (a -> k)
  | forall a. Method CallStack String [String] (m a) (a -> k)
  | forall a. Open String (m a) (a -> k)
  | Metaclass CallStack String k
  | Inherit String k
  | Frame k
  | Call String String [String] k
  | New String k
  | Var String k

deriving instance Functor (Preface m)

instance HFunctor Preface where
  hmap f (Def s n m k) = Def s n (f m) k
  hmap f (Klass s n m k) = Klass s n (f m) k
  hmap f (Method s n ps m k) = Method s n ps (f m) k
  hmap f (Open s m k) = Open s (f m) k
  hmap _ x = unsafeCoerce x -- FIXME

instance Effect Preface where
  handle state handler (Def s n m k) = Def s n (handler (m <$ state)) (handler . fmap k)
  handle state handler (Klass s n m k) = Klass s n (handler (m <$ state)) (handler . fmap k)
  handle state handler (Method s n ps m k) = Method s n ps (handler (m <$ state)) (handler . fmap k)
  handle state handler (Open s m k) = Open s (handler (m <$ state)) (handler . fmap k)

  handle state handler (Metaclass stack s k) = Metaclass stack s (handler (k <$ state))
  handle state handler (Inherit s k) = Inherit s (handler (k <$ state))
  handle state handler (Frame k) = Frame (handler (k <$ state))
  handle state handler (Call l r args k) = Call l r args (handler (k <$ state))
  handle state handler (New s k) = New s (handler (k <$ state))
  handle state handler (Var s k) = Var s (handler (k <$ state))

def :: (HasCallStack, Member Preface sig, Carrier sig m) => String -> m a -> m a
def n m = withFrozenCallStack $ send (Def callStack n m pure)

klass :: (HasCallStack, Member Preface sig, Carrier sig m) => String -> m a -> m a
klass n m = withFrozenCallStack $ send (Klass callStack n m pure)

method :: (HasCallStack, Member Preface sig, Carrier sig m) => String -> [String] -> m a -> m a
method n ps m = withFrozenCallStack $ send (Method callStack n ps m pure)

open :: (Member Preface sig, Carrier sig m) => String -> m a -> m a
open n m = send (Open n m pure)

metaclass :: (HasCallStack, Member Preface sig, Carrier sig m) => String -> m ()
metaclass n = withFrozenCallStack $ send (Metaclass callStack n (pure ()))

inherit :: (Member Preface sig, Carrier sig m) => String -> m ()
inherit n = send (Inherit n (pure ()))

frame :: (Member Preface sig, Carrier sig m) => m ()
frame = send (Frame (pure ()))

call :: (Member Preface sig, Carrier sig m) => String -> String -> [String] -> m ()
call l r args = send (Call l r args (pure ()))

new :: (Member Preface sig, Carrier sig m) => String -> m ()
new n = send (New n (pure ()))

var :: (Member Preface sig, Carrier sig m) => String -> m ()
var n = send (Var n (pure ()))

{-# LANGUAGE FunctionalDependencies, UndecidableInstances, ScopedTypeVariables #-}
module Control.Abstract.Primitive
  ( defineClass
  , defineNamespace
  ) where

import           Control.Abstract.Context
import           Control.Abstract.Environment
import           Control.Abstract.Evaluator
import           Control.Abstract.Heap
import           Control.Abstract.ScopeGraph (Declaration (..), EdgeLabel (..), ScopeError, ScopeGraph, currentScope, declare, newScope, withScope, Allocator)
import           Control.Abstract.Value
import           Data.Abstract.BaseError
import           Data.Abstract.Environment
import qualified Data.Abstract.Environment as Env
import           Data.Abstract.Name (Name)
import qualified Data.Abstract.Name as Name
import qualified Data.Map.Strict as Map
import           Data.Text (unpack)
import           Prologue

import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.Value
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Name
import Prologue

defineClass :: ( AbstractValue term address value m
               , Carrier sig m
               , HasCallStack
               , Member (Allocator address) sig
               , Member (Deref value) sig
               , Member (Reader ModuleInfo) sig
               , Member (Reader Span) sig
               , Member (State (Heap address address value)) sig
               , Member Fresh sig
               , Member (Resumable (BaseError (HeapError address))) sig
               , Member (Resumable (BaseError (ScopeError address))) sig
               , Member (State (Heap address address value)) sig
               , Member (State (ScopeGraph address)) sig
               , Ord address
               )
            => Declaration
            -> [value]
            -> Evaluator term address value m a
            -> Evaluator term address value m ()
defineClass declaration superclasses body = void . define declaration $ do
  withChildFrame declaration $ \frame -> do
    _ <- body
    klass declaration superclasses frame

defineNamespace :: ( AbstractValue term address value m
                   , Carrier sig m
                   , HasCallStack
                   , Member (Allocator address) sig
                   , Member (Deref value) sig
                   , Member (Env address) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Member (State (Heap address address value)) sig
                   , Member (Resumable (BaseError (HeapError address))) sig
                   , Member Fresh sig
                   , Member (Resumable (BaseError (ScopeError address))) sig
                   , Member (State (Heap address address value)) sig
                   , Member (State (ScopeGraph address)) sig
                   , Ord address
                   )
                => Declaration
                -> Evaluator term address value m a
                -> Evaluator term address value m ()
defineNamespace declaration body = void . define declaration $ do
  withChildFrame declaration $ \frame -> do
    _ <- body
    namespace declaration Nothing frame

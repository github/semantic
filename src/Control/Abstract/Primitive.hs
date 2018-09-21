{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
module Control.Abstract.Primitive
  ( define
  , defineClass
  , defineNamespace
  ) where

import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.Value
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Name
import Prologue

define :: ( HasCallStack
          , Member (Allocator address) effects
          , Member (Deref value) effects
          , Member (Env address) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader Span) effects
          , Member (State (Heap address value)) effects
          , Ord address
          )
       => Name
       -> Evaluator term address value effects value
       -> Evaluator term address value effects ()
define name def = withCurrentCallStack callStack $ do
  addr <- alloc name
  def >>= assign addr
  bind name addr

defineClass :: ( AbstractValue term address value effects
               , HasCallStack
               , Member (Allocator address) effects
               , Member (Deref value) effects
               , Member (Env address) effects
               , Member (Reader ModuleInfo) effects
               , Member (Reader Span) effects
               , Member (State (Heap address value)) effects
               , Ord address
               )
            => Name
            -> [address]
            -> Evaluator term address value effects a
            -> Evaluator term address value effects ()
defineClass name superclasses body = define name $ do
  binds <- Env.head <$> locally (body >> getEnv)
  klass name superclasses binds

defineNamespace :: ( AbstractValue term address value effects
                   , HasCallStack
                   , Member (Allocator address) effects
                   , Member (Deref value) effects
                   , Member (Env address) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   , Member (State (Heap address value)) effects
                   , Ord address
                   )
                => Name
                -> Evaluator term address value effects a
                -> Evaluator term address value effects ()
defineNamespace name scope = define name $ do
  binds <- Env.head <$> locally (scope >> getEnv)
  namespace name Nothing binds

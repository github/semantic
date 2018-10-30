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
          , Member (Allocator address) sig
          , Member (Deref value) sig
          , Member (Env address) sig
          , Member (Reader ModuleInfo) sig
          , Member (Reader Span) sig
          , Member (State (Heap address value)) sig
          , Carrier sig m
          , Ord address
          )
       => Name
       -> Evaluator term address value m value
       -> Evaluator term address value m ()
define name def = withCurrentCallStack callStack $ do
  addr <- alloc name
  def >>= assign addr
  bind name addr

defineClass :: ( AbstractValue term address value m
               , Carrier sig m
               , HasCallStack
               , Member (Allocator address) sig
               , Member (Deref value) sig
               , Member (Env address) sig
               , Member (Reader ModuleInfo) sig
               , Member (Reader Span) sig
               , Member (State (Heap address value)) sig
               , Ord address
               )
            => Name
            -> [address]
            -> Evaluator term address value m a
            -> Evaluator term address value m ()
defineClass name superclasses body = define name $ do
  binds <- Env.head <$> locally (body >> getEnv)
  klass name superclasses binds

defineNamespace :: ( AbstractValue term address value m
                   , Carrier sig m
                   , HasCallStack
                   , Member (Allocator address) sig
                   , Member (Deref value) sig
                   , Member (Env address) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Member (State (Heap address value)) sig
                   , Ord address
                   )
                => Name
                -> Evaluator term address value m a
                -> Evaluator term address value m ()
defineNamespace name scope = define name $ do
  binds <- Env.head <$> locally (scope >> getEnv)
  namespace name Nothing binds

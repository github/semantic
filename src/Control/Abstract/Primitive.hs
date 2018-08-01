module Control.Abstract.Primitive
  ( define
  , defineClass
  , defineNamespace
  , builtInPrint
  , builtInExport
  , lambda
  ) where

import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.Value
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Name
import Data.Text (unpack)
import Prologue

define :: ( HasCallStack
          , Member (Allocator address value) effects
          , Member (Env address) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader Span) effects
          )
       => Name
       -> Evaluator address value effects value
       -> Evaluator address value effects ()
define name def = withCurrentCallStack callStack $ do
  addr <- alloc name
  def >>= assign addr
  bind name addr

defineClass :: ( AbstractValue address value effects
               , HasCallStack
               , Member (Allocator address value) effects
               , Member (Env address) effects
               , Member (Reader ModuleInfo) effects
               , Member (Reader Span) effects
               )
            => Name
            -> [address]
            -> Evaluator address value effects a
            -> Evaluator address value effects ()
defineClass name superclasses body = define name $ do
  binds <- Env.head <$> locally (body >> getEnv)
  klass name superclasses binds

defineNamespace :: ( AbstractValue address value effects
                   , HasCallStack
                   , Member (Allocator address value) effects
                   , Member (Env address) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   )
                => Name
                -> Evaluator address value effects a
                -> Evaluator address value effects ()
defineNamespace name scope = define name $ do
  binds <- Env.head <$> locally (scope >> getEnv)
  namespace name Nothing binds

lambda :: ( HasCallStack
          , Member Fresh effects
          , Member (Function address value) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader Span) effects
          )
       => (Name -> Evaluator address value effects address)
       -> Evaluator address value effects value
lambda body = withCurrentCallStack callStack $ do
  var <- gensym
  function [var] lowerBound (body var)

builtInPrint :: ( AbstractValue address value effects
                , HasCallStack
                , Member (Allocator address value) effects
                , Member (Deref address value) effects
                , Member (Env address) effects
                , Member Fresh effects
                , Member (Function address value) effects
                , Member (Reader ModuleInfo) effects
                , Member (Reader Span) effects
                , Member (Resumable (EnvironmentError address)) effects
                , Member Trace effects
                )
             => Evaluator address value effects value
builtInPrint = lambda (\ v -> variable v >>= deref >>= asString >>= trace . unpack >> box unit)

builtInExport :: ( AbstractValue address value effects
                 , HasCallStack
                 , Member (Allocator address value) effects
                 , Member (Deref address value) effects
                 , Member (Env address) effects
                 , Member Fresh effects
                 , Member (Function address value) effects
                 , Member (Reader ModuleInfo) effects
                 , Member (Reader Span) effects
                 , Member (Resumable (EnvironmentError address)) effects
                 )
              => Evaluator address value effects value
builtInExport = lambda (\ v -> do
  var <- variable v >>= deref
  (k, value) <- asPair var
  sym <- asString k
  addr <- box value
  export (name sym) (name sym) (Just addr)
  box unit)

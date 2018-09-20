{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
module Control.Abstract.Primitive
  ( define
  , defineClass
  , defineNamespace
  , builtInPrint
  , builtInExport
  , lambda
  , Lambda(..)
  ) where

import Data.Abstract.Environment
import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.Value
import qualified Data.Abstract.Environment as Env
import Data.Abstract.BaseError
import Data.Abstract.Name
import Data.Text (unpack)
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

defineClass :: ( AbstractValue address value effects
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

defineNamespace :: ( AbstractValue address value effects
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

-- | Construct a function from a Haskell function taking 'Name's as arguments.
--
-- The constructed function will have the same arity as the Haskell function. Nullary functions are constructed by providing an evaluator producing an address. Note that the constructed function must not contain free variables as they will not be captured by the closure, and/or will be garbage collected.
lambda :: ( HasCallStack
          , Lambda term address value effects fn
          , Member (Reader ModuleInfo) effects
          , Member (Reader Span) effects
          )
       => fn
       -> Evaluator term address value effects value
lambda body = withCurrentCallStack callStack (lambda' [] body)

-- | A class of types forming the body of 'lambda's. Note that there should in general only be two cases: a recursive case of functions taking 'Name's as parameters, and a base case of an 'Evaluator'.
class Lambda term address value effects ty | ty -> term, ty -> address, ty -> value, ty -> effects where
  lambda' :: [Name]
          -> ty
          -> Evaluator term address value effects value

instance (Member Fresh effects, Lambda term address value effects ret) => Lambda term address value effects (Name -> ret) where
  lambda' vars body = do
    var <- gensym
    lambda' (var : vars) (body var)
  {-# INLINE lambda' #-}

instance Member (Function address value) effects => Lambda term address value effects (Evaluator term address value effects address) where
  lambda' vars = function Nothing vars lowerBound
  {-# INLINE lambda' #-}

builtInPrint :: ( AbstractValue address value effects
                , HasCallStack
                , Member (Allocator address) effects
                , Member (Deref value) effects
                , Member (Env address) effects
                , Member Fresh effects
                , Member (Function address value) effects
                , Member (Reader ModuleInfo) effects
                , Member (Reader Span) effects
                , Member (Resumable (BaseError (AddressError address value))) effects
                , Member (Resumable (BaseError (EnvironmentError address))) effects
                , Member (State (Heap address value)) effects
                , Member Trace effects
                , Ord address
                )
             => Evaluator term address value effects value
builtInPrint = lambda (\ v -> variable v >>= deref >>= asString >>= trace . unpack >> box unit)

builtInExport :: ( AbstractValue address value effects
                 , HasCallStack
                 , Member (Allocator address) effects
                 , Member (Deref value) effects
                 , Member (Env address) effects
                 , Member Fresh effects
                 , Member (Function address value) effects
                 , Member (Reader ModuleInfo) effects
                 , Member (Reader Span) effects
                 , Member (Resumable (BaseError (AddressError address value))) effects
                 , Member (Resumable (BaseError (EnvironmentError address))) effects
                 , Member (State (Heap address value)) effects
                 , Ord address
                 )
              => Evaluator term address value effects value
builtInExport = lambda (\ v -> do
  var <- variable v >>= deref
  (k, value) <- asPair var
  sym <- asString k
  addr <- box value
  export (name sym) (name sym) (Just addr)
  box unit)

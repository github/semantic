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

import           Control.Abstract.Context
import           Control.Abstract.Environment
import           Control.Abstract.Evaluator
import           Control.Abstract.Heap
import           Control.Abstract.ScopeGraph
    (Declaration (..), EdgeLabel (..), ScopeError, ScopeGraph, currentScope, declare, newScope, withScope)
import           Control.Abstract.Value
import           Data.Abstract.BaseError
import           Data.Abstract.Environment
import qualified Data.Abstract.Environment as Env
import           Data.Abstract.Name (Name)
import qualified Data.Abstract.Name as Name
import qualified Data.Map.Strict as Map
import           Data.Text (unpack)
import           Prologue

define :: ( HasCallStack
          , Member (Allocator (Address address)) effects
          , Member (Deref value) effects
          , Member (Env address) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader Span) effects
          , Member (State (Heap address address value)) effects
          , Member (State (ScopeGraph address)) effects
          , Member (Resumable (BaseError (ScopeError address))) effects
          , Ord address
          )
       => Declaration
       -> Evaluator address value effects value
       -> Evaluator address value effects ()
define declaration def = withCurrentCallStack callStack $ do
  span <- ask @Span -- TODO: This Span is most definitely wrong
  addr <- declare declaration span Nothing
  def >>= assign addr
  -- TODO: This probably needs to declare something in the scope graph.
  bind (name declaration) (Control.Abstract.Heap.address addr) -- TODO: Insert something in the heap

defineClass :: ( AbstractValue address value effects
               , HasCallStack
               , Member (Allocator (Address address)) effects
               , Member (Deref value) effects
               , Member (Env address) effects
               , Member (Reader ModuleInfo) effects
               , Member (Reader Span) effects
               , Member (Resumable (BaseError (ScopeError address))) effects
               , Member (State (Heap address address value)) effects
               , Member (State (ScopeGraph address)) effects
               , Ord address
               )
            => Declaration
            -> [address]
            -> Evaluator address value effects a
            -> Evaluator address value effects ()
defineClass declaration superclasses body = define declaration $ do
  binds <- Env.head <$> locally (body >> getEnv)
  klass declaration superclasses binds

defineNamespace :: ( AbstractValue address value effects
                   , HasCallStack
                   , Member (Allocator (Address address)) effects
                   , Member (Deref value) effects
                   , Member (Env address) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   , Member (Resumable (BaseError (ScopeError address))) effects
                   , Member (State (Heap address address value)) effects
                   , Member (State (ScopeGraph address)) effects
                   , Ord address
                   )
                => Declaration
                -> Evaluator address value effects a
                -> Evaluator address value effects ()
defineNamespace declaration scope = define declaration $ do
  binds <- Env.head <$> locally (scope >> getEnv)
  namespace declaration Nothing binds

-- | Construct a function from a Haskell function taking 'Name's as arguments.
--
-- The constructed function will have the same arity as the Haskell function. Nullary functions are constructed by providing an evaluator producing an address. Note that the constructed function must not contain free variables as they will not be captured by the closure, and/or will be garbage collected.
lambda :: ( HasCallStack
          , Lambda address value effects fn
          , Member (Reader ModuleInfo) effects
          , Member (Reader Span) effects
          )
       => fn
       -> Evaluator address value effects value
lambda body = withCurrentCallStack callStack (lambda' [] body)

-- | A class of types forming the body of 'lambda's. Note that there should in general only be two cases: a recursive case of functions taking 'Name's as parameters, and a base case of an 'Evaluator'.
class Lambda address value effects ty | ty -> address, ty -> value, ty -> effects where
  lambda' :: [Name]
          -> ty
          -> Evaluator address value effects value

instance (Member Fresh effects, Lambda address value effects ret) => Lambda address value effects (Name -> ret) where
  lambda' vars body = do
    var <- Name.gensym
    lambda' (var : vars) (body var)
  {-# INLINE lambda' #-}

instance (Member Fresh effects, Member (Function address value) effects, Member (State (ScopeGraph address)) effects) => Lambda address value effects (Evaluator address value effects address) where
  lambda' vars action = do
    name <- Name.gensym
    span <- ask @Span -- TODO: This span is probably wrong
    currentScope' <- currentScope
    address <- declare (Declaration name) span Nothing
    let edges = maybe mempty (Map.singleton Lexical . pure) currentScope'
    functionScope <- newScope edges

    functionFrame <- newFrame functionScope edges
    withFrame functionFrame $
      function name vars lowerBound action
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
                , Member (State (Heap address address value)) effects
                , Member Trace effects
                , Ord address
                )
             => Evaluator address value effects value
-- TODO: This Declaration usage might be wrong. How do we know name exists.
builtInPrint =
  lambda (\ v -> variable v >>= deref >>= asString >>= trace . unpack >> currentFrame) -- box unit)

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
                 , Member (State (Heap address address value)) effects
                 , Ord address
                 )
              => Evaluator address value effects value
builtInExport = lambda (\ v -> do
  var <- variable v >>= deref
  (k, value) <- asPair var
  sym <- asString k
  addr <- box value
  export (Name.name sym) (Name.name sym) (Just addr)
  box unit)

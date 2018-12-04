{-# LANGUAGE FunctionalDependencies, UndecidableInstances, ScopedTypeVariables, TupleSections #-}
module Control.Abstract.Primitive
  ( defineClass
  , defineNamespace
  ) where

import           Control.Abstract.Context
import           Control.Abstract.Evaluator
import           Control.Abstract.Heap
import           Control.Abstract.ScopeGraph
import           Control.Abstract.Value
import           Data.Abstract.BaseError
import Data.Map.Strict as Map
import           Prologue

defineClass :: ( AbstractValue term address value m
               , Carrier sig m
               , HasCallStack
               , Member (Allocator address) sig
               , Member (Deref value) sig
               , Member (Reader ModuleInfo) sig
               , Member (Reader Span) sig
               , Member Fresh sig
               , Member (Resumable (BaseError (HeapError address))) sig
               , Member (Resumable (BaseError (ScopeError address))) sig
               , Member (State (Heap address address value)) sig
               , Member (State (ScopeGraph address)) sig
               , Member (Reader (address, address)) sig
               , Ord address
               )
            => Declaration
            -> [Declaration]
            -> Evaluator term address value m a
            -> Evaluator term address value m ()
defineClass declaration superclasses body = void . define declaration $ do
    currentScope' <- currentScope

    superScopes <- for superclasses $ \superclass -> do
      scope <- associatedScope superclass
      pure scope

    let superclassEdges = (Superclass, ) <$> (fmap pure . catMaybes $ superScopes)
        current = fmap (Lexical, ) . pure . pure $ currentScope'
        edges = Map.fromList (superclassEdges <> current)
    childScope <- newScope edges
    putDeclarationScope declaration childScope

    withScope childScope $ do
      void $ body

    pure unit

defineNamespace :: ( AbstractValue term address value m
                   , Carrier sig m
                   , HasCallStack
                   , Member (Allocator address) sig
                   , Member (Deref value) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Member (Resumable (BaseError (HeapError address))) sig
                   , Member Fresh sig
                   , Member (Resumable (BaseError (ScopeError address))) sig
                   , Member (State (Heap address address value)) sig
                   , Member (State (ScopeGraph address)) sig
                   , Member (Reader (address, address)) sig
                   , Ord address
                   )
                => Declaration
                -> Evaluator term address value m a
                -> Evaluator term address value m ()
defineNamespace declaration body = void . define declaration $ do
  withChildFrame declaration $ \frame -> do
    _ <- body
    namespace declaration Nothing frame

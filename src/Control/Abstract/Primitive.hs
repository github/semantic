{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Control.Abstract.Primitive
  ( defineClass
  , defineNamespace
  , defineBuiltIn
  ) where

import           Analysis.Name
import           Control.Abstract.Context
import           Control.Abstract.Evaluator
import           Control.Abstract.Heap
import           Control.Abstract.ScopeGraph
import           Control.Abstract.Value
import           Control.Monad
import           Data.Abstract.BaseError
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Map.Strict as Map
import           Data.Maybe
import           Data.Semilattice.Lower
import           Data.Traversable
import           GHC.Stack

defineBuiltIn :: ( HasCallStack
                 , Has (Deref value) sig m
                 , Has (Reader (CurrentFrame address)) sig m
                 , Has (Reader (CurrentScope address)) sig m
                 , Has (Reader ModuleInfo) sig m
                 , Has (Reader Span) sig m
                 , Has (State (Heap address address value)) sig m
                 , Has (State (ScopeGraph address)) sig m
                 , Has (Resumable (BaseError (ScopeError address))) sig m
                 , Has (Resumable (BaseError (HeapError address))) sig m
                 , Has (Function term address value) sig m
                 , Has (Allocator address) sig m
                 , Has Fresh sig m
                 , Ord address
                 )
              => Declaration
              -> Relation
              -> AccessControl
              -> BuiltIn
              -> Evaluator term address value m ()
defineBuiltIn declaration rel accessControl value = withCurrentCallStack callStack $ do
  currentScope' <- currentScope
  let lexicalEdges = Map.singleton Lexical [ currentScope' ]
  associatedScope <- newPreludeScope lexicalEdges
  -- TODO: This span is still wrong.
  declare declaration rel accessControl lowerBound ScopeGraph.Unknown (Just associatedScope)

  withScope associatedScope $ do
    param <- gensym
    declare (Declaration param) ScopeGraph.Gensym accessControl lowerBound ScopeGraph.Unknown Nothing

  slot <- lookupSlot declaration
  value <- builtIn associatedScope value
  assign slot value

defineClass :: ( HasCallStack
               , Has (Allocator address) sig m
               , Has (Deref value) sig m
               , Has (Reader ModuleInfo) sig m
               , Has (Reader Span) sig m
               , Has Fresh sig m
               , Has (Reader (CurrentFrame address)) sig m
               , Has (Reader (CurrentScope address)) sig m
               , Has (Resumable (BaseError (HeapError address))) sig m
               , Has (Resumable (BaseError (ScopeError address))) sig m
               , Has (State (Heap address address value)) sig m
               , Has (State (ScopeGraph address)) sig m
               , Has (Unit value) sig m
               , Ord address
               )
            => Declaration
            -> [Declaration]
            -> Evaluator term address value m a
            -> Evaluator term address value m ()
defineClass declaration superclasses body = void . define declaration Default Public $ do
    currentScope' <- currentScope

    superScopes <- for superclasses associatedScope

    let superclassEdges = (Superclass, ) <$> (fmap pure . catMaybes $ superScopes)
        current = fmap (Lexical, ) . pure . pure $ currentScope'
        edges = Map.fromList (superclassEdges <> current)
    childScope <- newPreludeScope edges
    putDeclarationScope declaration childScope

    withScope childScope $ do
      void body

    unit

defineNamespace :: ( AbstractValue term address value m
                   , HasCallStack
                   , Has (Allocator address) sig m
                   , Has (Deref value) sig m
                   , Has (Reader (CurrentFrame address)) sig m
                   , Has (Reader (CurrentScope address)) sig m
                   , Has (Reader ModuleInfo) sig m
                   , Has (Reader Span) sig m
                   , Has (Resumable (BaseError (HeapError address))) sig m
                   , Has Fresh sig m
                   , Has (Resumable (BaseError (ScopeError address))) sig m
                   , Has (State (Heap address address value)) sig m
                   , Has (State (ScopeGraph address)) sig m
                   , Ord address
                   )
                => Declaration
                -> Evaluator term address value m a
                -> Evaluator term address value m ()
defineNamespace declaration@Declaration{..} body = void . define declaration Default Public $ do
  withChildFrame declaration $ \frame -> do
    _ <- body
    namespace unDeclaration frame

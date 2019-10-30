{-# LANGUAGE FlexibleContexts, RecordWildCards, TupleSections #-}
module Control.Abstract.Primitive
  ( defineClass
  , defineNamespace
  , defineBuiltIn
  ) where

import Control.Abstract.Context
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.ScopeGraph
import Control.Abstract.Value
import Data.Abstract.BaseError
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import Data.Abstract.Name
import Data.Map.Strict as Map
import Prologue

defineBuiltIn :: ( HasCallStack
                 , Member (Deref value) sig
                 , Member (Reader (CurrentFrame address)) sig
                 , Member (Reader (CurrentScope address)) sig
                 , Member (Reader ModuleInfo) sig
                 , Member (Reader Span) sig
                 , Member (State (Heap address address value)) sig
                 , Member (State (ScopeGraph address)) sig
                 , Member (Resumable (BaseError (ScopeError address))) sig
                 , Member (Resumable (BaseError (HeapError address))) sig
                 , Member (Function term address value) sig
                 , Member (Allocator address) sig
                 , Member Fresh sig
                 , Ord address
                 , Carrier sig m
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

defineClass :: ( Carrier sig m
               , HasCallStack
               , Member (Allocator address) sig
               , Member (Deref value) sig
               , Member (Reader ModuleInfo) sig
               , Member (Reader Span) sig
               , Member Fresh sig
               , Member (Reader (CurrentFrame address)) sig
               , Member (Reader (CurrentScope address)) sig
               , Member (Resumable (BaseError (HeapError address))) sig
               , Member (Resumable (BaseError (ScopeError address))) sig
               , Member (State (Heap address address value)) sig
               , Member (State (ScopeGraph address)) sig
               , Member (Unit value) sig
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
                   , Carrier sig m
                   , HasCallStack
                   , Member (Allocator address) sig
                   , Member (Deref value) sig
                   , Member (Reader (CurrentFrame address)) sig
                   , Member (Reader (CurrentScope address)) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
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
defineNamespace declaration@Declaration{..} body = void . define declaration Default Public $ do
  withChildFrame declaration $ \frame -> do
    _ <- body
    namespace unDeclaration frame

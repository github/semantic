{-# LANGUAGE FunctionalDependencies, UndecidableInstances, ScopedTypeVariables, TupleSections #-}
module Control.Abstract.Primitive
  ( defineClass
  , defineNamespace
  , defineBuiltIn
  ) where

import           Control.Abstract.Context
import           Control.Abstract.Evaluator
import           Control.Abstract.Heap
import           Control.Abstract.ScopeGraph
import           Control.Abstract.Value
import           Data.Abstract.BaseError
import Data.Map.Strict as Map
import Data.Abstract.Ref
import Data.Abstract.Name
import Data.Span
import           Prologue

defineBuiltIn :: forall value sig address m term. ( HasCallStack
          , Member (Deref value) sig
          , Member (Reader ModuleInfo) sig
          , Member (Reader Span) sig
          , Member (Reader (address, address)) sig
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
       -> BuiltIn
       -> Evaluator term address value m (ValueRef address value)
defineBuiltIn declaration value = withCurrentCallStack callStack $ do
  currentScope' <- currentScope
  let lexicalEdges = Map.singleton Lexical [ currentScope' ]
  associatedScope <- newScope lexicalEdges
  -- TODO: This span is still wrong.
  declare declaration emptySpan (Just associatedScope)

  param <- gensym
  withScope associatedScope $ do
    declare (Declaration param) emptySpan Nothing

  slot <- lookupDeclaration declaration
  value <- builtIn associatedScope value
  LvalMember slot <$ assign slot value

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
defineNamespace declaration@Declaration{..} body = void . define declaration $ do
  withChildFrame declaration $ \frame -> do
    _ <- body
    namespace unDeclaration frame

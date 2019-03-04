{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.TypeScript.Syntax.JavaScript where

import Prologue

import           Control.Abstract.Heap
import           Control.Abstract.ScopeGraph hiding (Import)
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.JSON.Fields
import qualified Data.Map.Strict as Map
import           Diffing.Algorithm
import           Language.TypeScript.Resolution

data JavaScriptRequire a = JavaScriptRequire { javascriptRequireIden :: !a, javascriptRequireFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically JavaScriptRequire

instance Evaluatable JavaScriptRequire where
  eval _ _ (JavaScriptRequire aliasTerm importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath javascriptExtensions
    ((moduleScope, moduleFrame), _) <- require modulePath

    case declaredName aliasTerm of
      Just alias -> do
        span <- ask @Span
        importScope <- newScope (Map.singleton ScopeGraph.Import [ moduleScope ])
        declare (Declaration alias) Default Public span ScopeGraph.UnqualifiedImportKind (Just importScope)
        let scopeMap = Map.singleton moduleScope moduleFrame
        aliasFrame <- newFrame importScope (Map.singleton ScopeGraph.Import scopeMap)
        aliasSlot <- lookupSlot (Declaration alias)
        assign aliasSlot =<< object aliasFrame
      Nothing -> do
        insertImportEdge moduleScope
        insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    unit

data Debugger a = Debugger
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Debugger

instance Evaluatable Debugger

data Super a = Super
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Super

instance Evaluatable Super

data Undefined a = Undefined
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Undefined

instance Evaluatable Undefined

data With a = With { withExpression :: !a, withBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically With

instance Evaluatable With

-- | A sequence expression such as Javascript or C's comma operator.
data AnnotatedExpression a = AnnotatedExpression { expression :: !a, typeAnnotation :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically AnnotatedExpression

instance Evaluatable AnnotatedExpression where
  eval eval _ (AnnotatedExpression a b) = eval b >> eval a

{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.TypeScript.Syntax.JavaScript where

import Prologue

import           Proto3.Suite

import           Data.Abstract.Evaluatable
import           Data.JSON.Fields
import           Diffing.Algorithm
import           Language.TypeScript.Resolution
import           Control.Abstract.ScopeGraph hiding (Import)
import           Control.Abstract.Heap
import           qualified Data.Abstract.ScopeGraph as ScopeGraph
import           qualified Data.Map.Strict as Map

data JavaScriptRequire a = JavaScriptRequire { javascriptRequireIden :: !a, javascriptRequireFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JavaScriptRequire where liftEq = genericLiftEq
instance Ord1 JavaScriptRequire where liftCompare = genericLiftCompare
instance Show1 JavaScriptRequire where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JavaScriptRequire where
  eval _ (JavaScriptRequire aliasTerm importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath javascriptExtensions
    ((moduleScope, moduleFrame), _) <- require modulePath

    case declaredName aliasTerm of
      Just alias -> do
        span <- ask @Span
        importScope <- newScope (Map.singleton ScopeGraph.Import [ moduleScope ])
        declare (Declaration alias) span (Just importScope)
        let scopeMap = Map.singleton moduleScope moduleFrame
        aliasFrame <- newFrame importScope (Map.singleton ScopeGraph.Import scopeMap)
        aliasSlot <- lookupDeclaration (Declaration alias)
        assign aliasSlot =<< object aliasFrame
      Nothing -> do
        insertImportEdge moduleScope
        insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    rvalBox unit

data Debugger a = Debugger
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Debugger where liftEq = genericLiftEq
instance Ord1 Debugger where liftCompare = genericLiftCompare
instance Show1 Debugger where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Debugger

data Super a = Super
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Super where liftEq = genericLiftEq
instance Ord1 Super where liftCompare = genericLiftCompare
instance Show1 Super where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Super

data Undefined a = Undefined
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Undefined where liftEq = genericLiftEq
instance Ord1 Undefined where liftCompare = genericLiftCompare
instance Show1 Undefined where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Undefined

data With a = With { withExpression :: !a, withBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 With where liftEq = genericLiftEq
instance Ord1 With where liftCompare = genericLiftCompare
instance Show1 With where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable With

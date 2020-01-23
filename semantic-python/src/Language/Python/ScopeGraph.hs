{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Python.ScopeGraph
  ( scopeGraphModule
  ) where

import           AST.Element
import           Control.Algebra (Algebra (..), handleCoercible)
import           Control.Effect.Fresh
import           Control.Effect.Sketch
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Name as Name
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Traversable
import           GHC.Records
import           GHC.TypeLits
import           Language.Python.Patterns
import           ScopeGraph.Convert (Result (..), complete, todo)
import           Source.Loc
import qualified TreeSitter.Python.AST as Py

-- This orphan instance will perish once it lands in fused-effects.
instance Algebra sig m => Algebra sig (Ap m) where
  alg = Ap . alg . handleCoercible

-- This typeclass is internal-only, though it shares the same interface
-- as the one defined in semantic-scope-graph. The somewhat-unconventional
-- quantified constraint is to avoid having to define Show1 instances for
-- every single Python AST type.
class (forall a . Show a => Show (t a)) => ToScopeGraph t where
  scopeGraph ::
    ( Has Sketch sig m
    , Monoid (m Result)
    )
    => t Loc
    -> m Result

instance (ToScopeGraph l, ToScopeGraph r) => ToScopeGraph (l :+: r) where
  scopeGraph (L1 l) = scopeGraph l
  scopeGraph (R1 r) = scopeGraph r

onField ::
  forall (field :: Symbol) syn sig m r .
  ( Has Sketch sig m
  , HasField field (r Loc) (syn Loc)
  , ToScopeGraph syn
  , Monoid (m Result)
  )
  => r Loc
  -> m Result
onField
  = scopeGraph @syn
  . getField @field

onChildren ::
  ( Traversable t
  , ToScopeGraph syn
  , Has Sketch sig m
  , HasField "extraChildren" (r Loc) (t (syn Loc))
  , Monoid (m Result)
  )
  => r Loc
  -> m Result
onChildren
  = fmap fold
  . traverse scopeGraph
  . getField @"extraChildren"

scopeGraphModule :: Has Sketch sig m => Py.Module Loc -> m Result
scopeGraphModule = getAp . scopeGraph

instance ToScopeGraph Py.AssertStatement where scopeGraph = onChildren

instance ToScopeGraph Py.Assignment where
  scopeGraph (Py.Assignment _ (SingleIdentifier t) _val _typ) = do
    let declProps = (DeclProperties ScopeGraph.Identifier ScopeGraph.Default Nothing)
    complete <* declare (Name.name t) declProps
  scopeGraph x                                                = todo x

instance ToScopeGraph Py.Await where
  scopeGraph (Py.Await _ a) = scopeGraph a

instance ToScopeGraph Py.BooleanOperator where
  scopeGraph (Py.BooleanOperator _ _ left right) = scopeGraph left <> scopeGraph right

instance ToScopeGraph Py.BinaryOperator where
  scopeGraph (Py.BinaryOperator _ _ left right) = scopeGraph left <> scopeGraph right

instance ToScopeGraph Py.AugmentedAssignment where scopeGraph = onField @"right"

instance ToScopeGraph Py.Attribute where scopeGraph = todo

instance ToScopeGraph Py.Block where scopeGraph = onChildren

instance ToScopeGraph Py.BreakStatement where scopeGraph = mempty

instance ToScopeGraph Py.Call where
  scopeGraph Py.Call
    { function
    , arguments = L1 Py.ArgumentList { extraChildren = args }
    } = do
      result <- scopeGraph function
      let scopeGraphArg = \case
            Prj expr -> scopeGraph @Py.Expression expr
            other    -> todo other
      args <- traverse scopeGraphArg args
      pure (result <> mconcat args)


instance ToScopeGraph Py.ClassDefinition where scopeGraph = todo

instance ToScopeGraph Py.ConcatenatedString where scopeGraph = mempty

deriving instance ToScopeGraph Py.CompoundStatement

instance ToScopeGraph Py.ConditionalExpression where scopeGraph = onChildren

instance ToScopeGraph Py.ContinueStatement where scopeGraph = mempty

instance ToScopeGraph Py.DecoratedDefinition where scopeGraph = todo

instance ToScopeGraph Py.ComparisonOperator where scopeGraph = onChildren

instance ToScopeGraph Py.DeleteStatement where scopeGraph = mempty

instance ToScopeGraph Py.Dictionary where scopeGraph = onChildren

instance ToScopeGraph Py.DictionaryComprehension where scopeGraph = todo

instance ToScopeGraph Py.DictionarySplat where scopeGraph = todo

deriving instance ToScopeGraph Py.Expression

instance ToScopeGraph Py.ElseClause where scopeGraph = onField @"body"

instance ToScopeGraph Py.ElifClause where
  scopeGraph (Py.ElifClause _ body condition) = scopeGraph condition <> scopeGraph body

instance ToScopeGraph Py.Ellipsis where scopeGraph = mempty

instance ToScopeGraph Py.ExceptClause where scopeGraph = onChildren

instance ToScopeGraph Py.ExecStatement where scopeGraph = mempty

instance ToScopeGraph Py.ExpressionStatement where scopeGraph = onChildren

instance ToScopeGraph Py.ExpressionList where scopeGraph = onChildren

instance ToScopeGraph Py.False where scopeGraph _ = pure mempty

instance ToScopeGraph Py.FinallyClause where scopeGraph = onField @"extraChildren"

instance ToScopeGraph Py.Float where scopeGraph = mempty

instance ToScopeGraph Py.ForStatement where scopeGraph = todo

instance ToScopeGraph Py.FunctionDefinition where
  scopeGraph Py.FunctionDefinition
    { name       = Py.Identifier _ann1 name
    , parameters = Py.Parameters _ann2 parameters
    , body
    } = do
    let funProps = FunProperties ScopeGraph.Function
    (_, associatedScope) <- declareFunction (Just $ Name.name name) funProps
    withScope associatedScope $ do
      let declProps = DeclProperties ScopeGraph.Parameter ScopeGraph.Default Nothing
      let param (Py.Parameter (Prj (Py.Identifier _pann pname))) = Just (Name.name pname)
          param _                                                = Nothing
      let parameterMs = fmap param parameters
      if any isNothing parameterMs
        then todo parameterMs
        else do
          let parameters' = catMaybes parameterMs
          paramDeclarations <- for parameters' $ \parameter ->
            complete <* declare parameter declProps
          bodyResult <- scopeGraph body
          pure (mconcat paramDeclarations <> bodyResult)

instance ToScopeGraph Py.FutureImportStatement where scopeGraph = todo

instance ToScopeGraph Py.GeneratorExpression where scopeGraph = todo

instance ToScopeGraph Py.Identifier where
  scopeGraph (Py.Identifier _ name) = do
    reference name name RefProperties
    complete

instance ToScopeGraph Py.IfStatement where
  scopeGraph (Py.IfStatement _ alternative body condition)
    = scopeGraph condition
    <> scopeGraph body
    <> foldMap scopeGraph alternative

instance ToScopeGraph Py.GlobalStatement where scopeGraph = todo

instance ToScopeGraph Py.Integer where scopeGraph = mempty

instance ToScopeGraph Py.ImportStatement where scopeGraph = todo

instance ToScopeGraph Py.ImportFromStatement where scopeGraph = todo

instance ToScopeGraph Py.Lambda where scopeGraph = todo

instance ToScopeGraph Py.List where scopeGraph = onChildren

instance ToScopeGraph Py.ListComprehension where scopeGraph = todo

instance ToScopeGraph Py.ListSplat where scopeGraph = onChildren

instance ToScopeGraph Py.NamedExpression where scopeGraph = todo

instance ToScopeGraph Py.None where scopeGraph = mempty

instance ToScopeGraph Py.NonlocalStatement where scopeGraph = todo

instance ToScopeGraph Py.Module where scopeGraph = onChildren

instance ToScopeGraph Py.ReturnStatement where
  scopeGraph (Py.ReturnStatement _ mVal) = maybe (pure mempty) scopeGraph mVal

instance ToScopeGraph Py.True where scopeGraph = mempty

instance ToScopeGraph Py.NotOperator where scopeGraph = onField @"argument"

instance ToScopeGraph Py.Pair where
  scopeGraph (Py.Pair _ value key) = scopeGraph key <> scopeGraph value

instance ToScopeGraph Py.ParenthesizedExpression where scopeGraph = onField @"extraChildren"

instance ToScopeGraph Py.PassStatement where scopeGraph = mempty

instance ToScopeGraph Py.PrintStatement where
  scopeGraph (Py.PrintStatement _ args _chevron) = foldMap scopeGraph args

deriving instance ToScopeGraph Py.PrimaryExpression

deriving instance ToScopeGraph Py.SimpleStatement

instance ToScopeGraph Py.RaiseStatement where scopeGraph = todo

instance ToScopeGraph Py.Set where scopeGraph = onChildren

instance ToScopeGraph Py.SetComprehension where scopeGraph = todo

instance ToScopeGraph Py.String where scopeGraph = mempty

instance ToScopeGraph Py.Subscript where scopeGraph = todo

instance ToScopeGraph Py.Tuple where scopeGraph = onChildren

instance ToScopeGraph Py.TryStatement where
  scopeGraph (Py.TryStatement _ body elseClauses)
    = scopeGraph body
    <> foldMap scopeGraph elseClauses

instance ToScopeGraph Py.UnaryOperator where scopeGraph = onField @"argument"

instance ToScopeGraph Py.WhileStatement where
  scopeGraph Py.WhileStatement{ alternative, body, condition }
    = scopeGraph condition
    <> scopeGraph body
    <> foldMap scopeGraph alternative

instance ToScopeGraph Py.WithStatement where
  scopeGraph = todo

instance ToScopeGraph Py.Yield where scopeGraph = onChildren

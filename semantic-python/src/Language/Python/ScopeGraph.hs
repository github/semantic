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

-- NOTE: This file needs to be updated to accommodate new AST shapes.
-- A portion of instances have been updated to include the Err functor; 
-- remaining instances are to be updated once this is stable.

module Language.Python.ScopeGraph
  ( scopeGraphModule
  ) where

import qualified Analysis.Name as Name
import           AST.Element
import qualified AST.Parse as Parse
import           Control.Effect.ScopeGraph
import qualified Control.Effect.ScopeGraph.Properties.Declaration as Props
import qualified Control.Effect.ScopeGraph.Properties.Function as Props
import qualified Control.Effect.ScopeGraph.Properties.Reference as Props
import           Control.Lens (set, (^.))
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe
import           Data.Monoid
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Traversable
import           GHC.Records
import           GHC.TypeLits
import qualified Language.Python.AST as Py
import           Language.Python.Patterns
import           Scope.Graph.Convert (Result (..), complete, todo)
import           Scope.Types
import           Source.Loc (Loc)
import           Source.Span (Span, Pos (..), span_, point)

-- This typeclass is internal-only, though it shares the same interface
-- as the one defined in semantic-scope-graph. The somewhat-unconventional
-- quantified constraint is to avoid having to define Show1 instances for
-- every single Python AST type.
class (forall a . Show a => Show (t a)) => ToScopeGraph t where
  scopeGraph ::
    ( ScopeGraphEff sig m
    , Monoid (m Result)
    )
    => t Loc
    -> m Result

instance (ToScopeGraph l, ToScopeGraph r) => ToScopeGraph (l :+: r) where
  scopeGraph (L1 l) = scopeGraph l
  scopeGraph (R1 r) = scopeGraph r

onField ::
  forall (field :: Symbol) syn sig m r .
  ( ScopeGraphEff sig m
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
  , ScopeGraphEff sig m
  , HasField "extraChildren" (r Loc) (t (syn Loc))
  , Monoid (m Result)
  )
  => r Loc
  -> m Result
onChildren
  = fmap fold
  . traverse scopeGraph
  . getField @"extraChildren"

scopeGraphModule :: ScopeGraphEff sig m => Py.Module Loc -> m Result
scopeGraphModule = getAp . scopeGraph

instance ToScopeGraph Py.AssertStatement where scopeGraph = onChildren

instance ToScopeGraph Py.Assignment where
  scopeGraph (Py.Assignment ann (Parse.Success (SingleIdentifier t)) val _typ) = do
    declare t Props.Declaration
      { Props.kind     = ScopeGraph.Assignment
      , Props.relation = ScopeGraph.Default
      , Props.associatedScope = Nothing
      , Props.span = ann^.span_
      }
    maybe complete scopeGraph val
  scopeGraph x = todo x

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
    { function = Parse.Success f
    , arguments = Parse.Success (L1 Py.ArgumentList { extraChildren = args })
    } = do
      result <- scopeGraph f
      let scopeGraphArg = \case
            EPrj expr -> scopeGraph @Py.Expression expr
            other    -> todo other
      args <- traverse scopeGraphArg args
      pure (result <> mconcat args)
  scopeGraph it = todo it


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
  scopeGraph (Py.ElifClause _ (Parse.Success body) (Parse.Success condition)) = scopeGraph condition <> scopeGraph body

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
    { ann
    , name       = Parse.Success (Py.Identifier _ann1 name)
    , parameters = Parse.Success (Py.Parameters _ann2 parameters)
    , body = Parse.Success b
    } = do
    (_, associatedScope) <- declareFunction (Just $ Name.name name) Props.Function
      { Props.kind = ScopeGraph.Function
      , Props.span = ann^.span_
      }
    withScope (CurrentScope associatedScope) $ do
      let declProps = Props.Declaration
            { Props.kind = ScopeGraph.Parameter
            , Props.relation = ScopeGraph.Default
            , Props.associatedScope = Nothing
            , Props.span = point (Pos 0 0)
            }
      let param (Py.Parameter (Prj (Py.Identifier pann pname))) = Just (pann, Name.name pname)
          param _                                               = Nothing
      let parameterMs = fmap param parameters
      if any isNothing parameterMs
        then todo parameterMs
        else do
          let parameters' = catMaybes parameterMs
          paramDeclarations <- for parameters' $ \(pos, parameter) ->
            complete <* declare parameter (set span_ (pos^.span_) declProps)
          bodyResult <- scopeGraph b
          pure (mconcat paramDeclarations <> bodyResult)

instance ToScopeGraph Py.FutureImportStatement where scopeGraph = todo

instance ToScopeGraph Py.GeneratorExpression where scopeGraph = todo

instance ToScopeGraph Py.Identifier where
  scopeGraph (Py.Identifier ann name) = do
    let refProps = Props.Reference ScopeGraph.Identifier ScopeGraph.Default (ann^.span_ :: Span)
    newReference (Name.name name) refProps
    complete

instance ToScopeGraph Py.IfStatement where
  scopeGraph (Py.IfStatement _ alternative (Parse.Success body) (Parse.Success condition))
    = scopeGraph condition
    <> scopeGraph body
    <> foldMap scopeGraph alternative

instance ToScopeGraph Py.GlobalStatement where scopeGraph = todo

instance ToScopeGraph Py.Integer where scopeGraph = mempty

instance ToScopeGraph Py.ImportStatement where
  scopeGraph (Py.ImportStatement _ ((R1 (Py.DottedName _ names@((Py.Identifier ann name) :| _))) :| [])) = do
    let toName (Py.Identifier _ name) = Name.name name
    newEdge ScopeGraph.Import (toName <$> names)

    let referenceProps = Props.Reference ScopeGraph.Identifier ScopeGraph.Default (ann^.span_ :: Span)
    newReference (Name.name name) referenceProps

    let pairs = zip (toList names) (tail $ toList names)
    for_ pairs $ \pair -> do
      case pair of
        (scopeIdentifier, referenceIdentifier@(Py.Identifier ann2 _)) -> do
          withScope (CurrentScope (toName scopeIdentifier)) $ do
            let referenceProps = Props.Reference ScopeGraph.Identifier ScopeGraph.Default (ann2^.span_ :: Span)
            newReference (toName referenceIdentifier) referenceProps

    complete
  scopeGraph term = todo (show term)

instance ToScopeGraph Py.ImportFromStatement where
  scopeGraph (Py.ImportFromStatement _ [] (L1 (Py.DottedName _ names)) (Just (Py.WildcardImport _ _))) = do
    let toName (Py.Identifier _ name) = Name.name name
    complete <* newEdge ScopeGraph.Import (toName <$> names)
  scopeGraph impossibleTerm@(Py.ImportFromStatement _ [] (L1 (Py.DottedName _ _)) Nothing) =
    todo impossibleTerm
  scopeGraph term = todo term


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

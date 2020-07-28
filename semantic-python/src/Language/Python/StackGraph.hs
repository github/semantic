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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE: This file needs to be updated to accommodate new AST shapes.
-- A portion of instances have been updated to include the Err functor;
-- remaining instances are to be updated once this is stable.

module Language.Python.StackGraph
  ( scopeGraphModule,
  )
where

import AST.Element
import AST.Element
import qualified AST.Parse as Parse
import qualified Analysis.Name as Name
import Control.Effect.Reader
import Control.Effect.StackGraph
import Control.Effect.State
import Control.Monad ((<=<))
import Data.Bifunctor
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Module (ModuleInfo)
import qualified Data.Module as Module
import qualified Data.ScopeGraph as ScopeGraph
import qualified Data.Text as Text
import Data.Traversable
import GHC.Records
import GHC.TypeLits
import qualified Language.Python.AST as Py
import Language.Python.Patterns
import qualified Proto.Semantic as P
import Scope.Graph.Convert (Result (..), complete, todo)
import Scope.Types as Scope
import Source.Loc (Loc)
import Source.Span (Pos (..), point)
import qualified Stack.Graph as Stack
import qualified System.Path as Path



-- This typeclass is internal-only, though it shares the same interface
-- as the one defined in semantic-scope-graph. The somewhat-unconventional
-- quantified constraint is to avoid having to define Show1 instances for
-- every single Python AST type.
class (forall a. Show a => Show (t a)) => ToScopeGraph t where

  scopeGraph ::
    (StackGraphEff sig m) =>
    t Loc ->
    m (BodyStruct Loc [Tagged Stack.Node])

instance (ToScopeGraph l, ToScopeGraph r) => ToScopeGraph (l :+: r) where
  scopeGraph (L1 l) = scopeGraph l
  scopeGraph (R1 r) = scopeGraph r

onField ::
  forall (field :: Symbol) syn sig m r.
  ( StackGraphEff sig m,
    HasField field (r Loc) (syn Loc),
    ToScopeGraph syn
  ) =>
  r Loc ->
  m (BodyStruct Loc [Tagged Stack.Node])
onField =
  scopeGraph @syn
    . getField @field

-- ([(node_for_the_name, ast_for_the_decl)], ...)
type ClassBodyBinder =
  Py.Assignment
    :+: Py.AugmentedAssignment
    :+: Py.FunctionDefinition
    :+: Py.ClassDefinition
    :+: Py.DecoratedDefinition

type BodyStruct a b = ([(Tagged Stack.Node, ClassBodyBinder a)], b)



--
--  The Nascent DSL
--


parentScopeNode name = do
  Py.Identifier _ann1 name0 <- ensureAST name
  parentScopeName <- pure $ Name.name (Text.pack "ParentScope " <> name0)
  internalScope parentScopeName

declarationNode :: (StackGraphEff sig m) => P.SyntaxType -> Loc -> Parse.Err (Py.Identifier a) -> m (Tagged Stack.Node)
declarationNode syntax ann name = do
  Py.Identifier _ann1 name' <- ensureAST name
  declare (Name.name name') syntax ann

formalParametersScopeNode :: (StackGraphEff sig m) => m (Tagged Stack.Node)
formalParametersScopeNode = scope (Name.name "FormalParameters")

--parameterNodes ::
parameterNodes parameters = do
  Py.Parameters _ann2 parameters0 <- ensureAST parameters
  parameters' <- mapM ensureAST parameters0
  parameterMs <- pure $ fmap param parameters'
  forM (zip [0 ..] parameterMs) $ \(ix, (pos, parameter)) ->
    declareParameter parameter ix P.UNKNOWN_SYNTAX pos
  where
      param (Py.Parameter (Prj (Py.Identifier pann pname))) = (pann, Name.name pname)
      param _ = error "Plz implement ScopeGraph.hs l223"


noBindings :: b -> BodyStruct a b
noBindings x = ([], x)

onlyBindings :: (StackGraphEff sig m) => m (BodyStruct Loc [Tagged Stack.Node]) -> m (BodyStruct Loc [Tagged Stack.Node])
onlyBindings m =
  do (bindings, _) <- m
     pure (bindings, [])

-- These next two are python specific which I don't like.
popNames :: (StackGraphEff sig m) => NonEmpty (Parse.Err (Py.Identifier Loc)) -> m (NonEmpty (Tagged Stack.Node))
popNames =
  mapM ((\(Py.Identifier _ name) -> popSymbol (Name.name name)) <=< ensureAST)

pushNames :: (StackGraphEff sig m) => NonEmpty (Parse.Err (Py.Identifier Loc)) -> m (NonEmpty (Tagged Stack.Node))
pushNames =
  mapM ((\(Py.Identifier _ name) -> pushSymbol (Name.name name)) <=< ensureAST)

childScopeGraph :: (ToScopeGraph t, StackGraphEff sig m) => Parse.Err (t Loc) -> m (BodyStruct Loc [Tagged Stack.Node])
childScopeGraph = scopeGraph <=< ensureAST

optionalChildScopeGraph :: (ToScopeGraph t, StackGraphEff sig m) => Maybe (Parse.Err (t Loc)) -> BodyStruct Loc [Tagged Stack.Node] -> m (BodyStruct Loc [Tagged Stack.Node])
optionalChildScopeGraph Nothing  n = pure n
optionalChildScopeGraph (Just x) _ = childScopeGraph x

aggregateChildScopeGraphs :: (ToScopeGraph t, StackGraphEff sig m) => [Parse.Err (t Loc)] -> m (BodyStruct Loc [Tagged Stack.Node])
aggregateChildScopeGraphs xs =
  do rs <- mapM childScopeGraph xs
     let (obs, ons) = unzip rs
     pure (concat obs, concat ons)

(<<>>) :: (Applicative f, Monoid a) => f a -> f a -> f a
x <<>> y = (<>) <$> x <*> y

optionalChildScopeGraph' :: (ToScopeGraph t, StackGraphEff sig m) => Maybe (Parse.Err (t Loc)) -> m (BodyStruct Loc [Tagged Stack.Node])
optionalChildScopeGraph' Nothing = pure ([], [])
optionalChildScopeGraph' (Just val) = childScopeGraph val

(~>>) :: (StackGraphEff sig m) => Tagged Stack.Node -> Tagged Stack.Node -> m ()
x ~>> y = modify (Stack.addEdge x y)

(~>>*) :: (StackGraphEff sig m) => Tagged Stack.Node -> [Tagged Stack.Node] -> m ()
x ~>>* ys = forM_ ys $ \y -> x ~>> y

(*~>>) :: (StackGraphEff sig m) => [Tagged Stack.Node] -> Tagged Stack.Node -> m ()
xs *~>> y = forM_ xs $ \x -> x ~>> y

(*~>>*) :: (StackGraphEff sig m) => [Tagged Stack.Node] -> [Tagged Stack.Node] -> m ()
xs *~>>* ys = forM_ xs $ \x -> forM_ ys $ \y -> x ~>> y

forwardPopMemberChain :: (StackGraphEff sig m) => NonEmpty (Tagged Stack.Node) -> m ()
forwardPopMemberChain (a :| []) = pure ()
forwardPopMemberChain (a :| (b : cs)) = do
  memberSymbol <- popSymbol "."
  a            ~>> memberSymbol
  memberSymbol ~>> b
  forwardPopMemberChain (b :| cs)

backwardPushMemberChain :: (StackGraphEff sig m) => NonEmpty (Tagged Stack.Node) -> m ()
backwardPushMemberChain (a :| []) = pure ()
backwardPushMemberChain (a :| (b : cs)) = do
  memberSymbol <- pushSymbol "."
  b            ~>> memberSymbol
  memberSymbol ~>> a
  backwardPushMemberChain (b :| cs)

returnScopeGraph :: (ToScopeGraph t, StackGraphEff sig m) => Maybe (Parse.Err (t Loc)) -> m (BodyStruct Loc [Tagged Stack.Node])
returnScopeGraph maybeVals = do
    -- TODO: Can probably be a newNodeWithName function
    returnNode <- scope (Name.name "R")
    CurrentScope currentScope' <- currentScope
    modify (Stack.addEdge returnNode currentScope')

    (bindings, nodes) <- optionalChildScopeGraph' maybeVals
    
    for_ nodes $ \node ->
      modify (Stack.addEdge returnNode node)
    
    pure ([], [returnNode])

scopeGraphModule :: StackGraphEff sig m => Py.Module Loc -> m ()
scopeGraphModule x = scopeGraph x >> pure ()

instance ToScopeGraph Py.AssertStatement where
  scopeGraph assertStmt = todo assertStmt

-- fmap (\x -> [x]) <$> todo assertStmt

instance ToScopeGraph Py.Assignment where
  scopeGraph asgn@(Py.Assignment _ _ Nothing _) =
    todo asgn
  scopeGraph asgn@(Py.Assignment ann (Parse.Success (SingleIdentifier identifier)) (Just (Parse.Success val)) _typ) = do
    -- TODO: What should we do with the type of an assignment?
    -- TODO: What should we do with the right hand side of an assignment?
    (propagateThese, _) <- scopeGraph val
    decl <- declare identifier P.UNKNOWN_SYNTAX ann
    pure ((decl, L1 asgn) : propagateThese, [decl])
  scopeGraph x = todo x

instance ToScopeGraph Py.Await where
  scopeGraph (Py.Await _ (Parse.Success a)) = scopeGraph a

instance ToScopeGraph Py.BooleanOperator where
  scopeGraph = todo -- (Py.BooleanOperator _ _ left right) = scopeGraph left <> scopeGraph right

instance ToScopeGraph Py.BinaryOperator where
  scopeGraph = todo -- (Py.BinaryOperator _ _ left right) = scopeGraph left <> scopeGraph right

instance ToScopeGraph Py.AugmentedAssignment where
  scopeGraph = todo

--(Py.AugmentedAssignment _ _ lhs rhs) = fmap _augmentedAssignment <$> onField @"right" x

instance ToScopeGraph Py.Attribute where
  scopeGraph = todo

instance ToScopeGraph Py.Block where
  scopeGraph (Py.Block _ statements) = aggregateChildScopeGraphs statements

instance ToScopeGraph Py.BreakStatement where
  scopeGraph statement = todo statement

instance ToScopeGraph Py.Call where
  scopeGraph
    Py.Call
      { function = Parse.Success f,
        arguments = Parse.Success (L1 Py.ArgumentList {extraChildren = args})
      } = do
      _result <- scopeGraph f
      let scopeGraphArg = \case
            EPrj expr -> scopeGraph @Py.Expression expr
            other -> todo other
      _args <- traverse scopeGraphArg args
      pure (todo ("Plz implement ScopeGraph.hs l164" :| [])) --(result <> mconcat args)
  scopeGraph it = todo it

instance ToScopeGraph Py.ClassDefinition where
  scopeGraph def@Py.ClassDefinition { ann, name,
        body } = do
      
      Py.Identifier _ann1 name' <- ensureAST name
      declaration <- declare (Name.name name') P.CLASS ann
      CurrentScope currentScope' <- currentScope
      callPopSymbol <- popSymbol "()"
      selfScope' <- selfScope "self"
      selfMemberPopSymbol <- popSymbol "."
      instanceMemberScope' <- instanceMemberScope "IM"
      classMemberScope' <- classMemberScope "CM"
      memberSymbol <- popSymbol "."
      
      currentScope'         ~>>  declaration
      declaration           ~>>  callPopSymbol
      callPopSymbol         ~>>  selfScope'
      selfScope'            ~>>  selfMemberPopSymbol
      selfMemberPopSymbol   ~>>  instanceMemberScope'
      instanceMemberScope'  ~>>  classMemberScope'
      declaration           ~>>  memberSymbol
      memberSymbol          ~>>  classMemberScope'

      -- TODO: Should we assert return nodes is empty here.
      (bindings, _) <- childScopeGraph body
      
      for_ bindings $ \(node, statement) ->
        if isInstanceMember statement
        then instanceMemberScope' ~>> node
        else if isClassMember statement
        then classMemberScope' ~>> node
        else pure ()

      -- TODO: replace R1L1 with injection
      pure ([(declaration, R1 (R1 (R1 (L1 def))))], [])

-- for_ bindings $ \(node, statement) -> do
--   -- let callNode = Stack.PopSymbol "()"
--   undefined

isInstanceMember :: ClassBodyBinder a -> Bool
isInstanceMember (Prj (Py.FunctionDefinition {})) = True
isInstanceMember _ = False

isClassMember :: ClassBodyBinder a -> Bool
isClassMember (Prj (Py.Assignment {})) = True
isClassMember (Prj (Py.ClassDefinition {})) = True
isClassMember (Prj (Py.DecoratedDefinition {})) = True
isClassMember _ = False

instance ToScopeGraph Py.ConcatenatedString where
  scopeGraph = todo

deriving instance ToScopeGraph Py.CompoundStatement

instance ToScopeGraph Py.ConditionalExpression where
  scopeGraph = todo

instance ToScopeGraph Py.ContinueStatement where
  scopeGraph _ = pure ([], [])

instance ToScopeGraph Py.DecoratedDefinition where
  scopeGraph = todo

instance ToScopeGraph Py.ComparisonOperator where
  scopeGraph = todo

instance ToScopeGraph Py.DeleteStatement where
  scopeGraph _ = pure ([], [])

instance ToScopeGraph Py.Dictionary where
  scopeGraph = todo

instance ToScopeGraph Py.DictionaryComprehension where
  scopeGraph = todo

instance ToScopeGraph Py.DictionarySplat where
  scopeGraph = todo

instance ToScopeGraph Py.Expression where
  scopeGraph = todo

instance ToScopeGraph Py.ElseClause where
  scopeGraph (Py.ElseClause _ body) =
    childScopeGraph body

instance ToScopeGraph Py.ElifClause where
  scopeGraph (Py.ElifClause _ body condition) =
    childScopeGraph condition >> childScopeGraph body

instance ToScopeGraph Py.Ellipsis where
  scopeGraph _ = pure ([], [])

instance ToScopeGraph Py.ExceptClause where
  scopeGraph x = todo x

instance ToScopeGraph Py.ExecStatement where
  scopeGraph x = todo x

instance ToScopeGraph Py.ExpressionStatement where
  scopeGraph (Py.ExpressionStatement _ eStatements) = 
    onlyBindings (aggregateChildScopeGraphs (toList eStatements))

instance ToScopeGraph Py.ExpressionList where
  scopeGraph = todo

instance ToScopeGraph Py.False where
  scopeGraph = todo

instance ToScopeGraph Py.FinallyClause where
  scopeGraph = todo

instance ToScopeGraph Py.Float where
  scopeGraph = todo

instance ToScopeGraph Py.ForStatement where
  scopeGraph = todo

instance ToScopeGraph Py.FunctionDefinition where

  scopeGraph
    Py.FunctionDefinition
      { ann,
        name = Parse.Success (Py.Identifier _ann1 name),
        parameters = Parse.Success (Py.Parameters _ann2 parameters),
        body = Parse.Success body
      } = do
      

      parameters' <- mapM ensureAST parameters
      name' <- pure $ Name.name name
      parentScopeName <- pure $ Name.name (Text.pack "ParentScope " <> name)
      parameterMs <- pure $ fmap param parameters'
      CurrentScope currentScope' <- currentScope
      declaration <- declare name' P.FUNCTION ann
      callPopSymbol <- popSymbol "()"
      formalParametersScope <- scope (Name.name "FormalParameters")
      parentScope <- internalScope parentScopeName
      (_, nodes) <- withScope parentScope $ scopeGraph body
      callNode <- popSymbol "()"
      (functionNameNode, _associatedScope) <- declareFunction (Just name') P.FUNCTION ann
      paramNodes <- forM (zip [0 ..] parameterMs) $ \(ix, (pos, parameter)) ->
        declareParameter parameter ix P.UNKNOWN_SYNTAX pos
      
      
      
      currentScope'          ~>>   declaration
      declaration            ~>>   callPopSymbol
      formalParametersScope  ~>>*  paramNodes
      parentScope            ~>>   formalParametersScope
      callNode               ~>>*  nodes
      functionNameNode       ~>>   callNode

      pure ([], [])
    
    where
      param (Py.Parameter (Prj (Py.Identifier pann pname))) = (pann, Name.name pname)
      param _ = error "Plz implement ScopeGraph.hs l223"

instance ToScopeGraph Py.FutureImportStatement where
  scopeGraph = todo

instance ToScopeGraph Py.GeneratorExpression where
  scopeGraph = todo

instance ToScopeGraph Py.Identifier where
  scopeGraph (Py.Identifier ann name) = do
    node <- refer (Name.name name) P.UNKNOWN_SYNTAX ann
    pure (noBindings [node])

instance ToScopeGraph Py.IfStatement where
  scopeGraph (Py.IfStatement _ alternatives body condition) =
    childScopeGraph condition >> (childScopeGraph body <<>> aggregateChildScopeGraphs alternatives)

instance ToScopeGraph Py.GlobalStatement where
  scopeGraph = todo

instance ToScopeGraph Py.Integer where
  scopeGraph = todo

instance ToScopeGraph Py.ImportStatement where
  scopeGraph (Py.ImportStatement _ (Parse.Success (R1 (Py.DottedName _ eNames@(Parse.Success (Py.Identifier ann definition) :| _))) :| [])) = do
    rootScope' <- rootScope
    ScopeGraph.CurrentScope previousScope <- currentScope

    importScope <- newScope previousScope

    names <- mapM ensureAST eNames
    let names' = (\(Py.Identifier ann name) -> (Name.name name, P.UNKNOWN_SYNTAX, ann)) <$> names
    (childGraph, initialReference) <- addDeclarations names'

    definitionScope <- declare (Name.name definition) P.UNKNOWN_SYNTAX ann
    let childGraph' = Stack.addEdge importScope definitionScope childGraph

    let childGraph'' = Stack.addEdge initialReference rootScope' childGraph'

    putCurrentScope importScope

    pure ([], [])
  scopeGraph term = todo (show term)

instance ToScopeGraph Py.ImportFromStatement where  
  scopeGraph term@(Py.ImportFromStatement _ [] (Parse.Success (L1 (Py.DottedName _ eNames))) (Just (Parse.Success (Py.WildcardImport _ _)))) = do
    todo term
  -- let toName (Py.Identifier _ name) = Name.name name
  -- names <- mapM ensureAST eNames
  -- complete <* newEdge ScopeGraph.Import (toName <$> names)
  scopeGraph (Py.ImportFromStatement _ _imports (Parse.Success (L1 (Py.DottedName _ _names@(Parse.Success (Py.Identifier _ann _scopeName) :| _)))) Nothing) = do
    -- let toName (Py.Identifier _ name) = Name.name name
    -- newEdge ScopeGraph.Import (toName <$> names)

    -- let referenceProps = Props.Reference ScopeGraph.Identifier ScopeGraph.Default (ann ^. span_ :: Span)
    -- newReference (Name.name scopeName) referenceProps

    -- let pairs = zip (toList names) (tail $ toList names)
    -- for_ pairs $ \pair -> do
    --   case pair of
    --     (scopeIdentifier, referenceIdentifier@(Py.Identifier ann2 _)) -> do
    --       withScope (toName scopeIdentifier) $ do
    --         let referenceProps = Props.Reference ScopeGraph.Identifier ScopeGraph.Default (ann2 ^. span_ :: Span)
    --         newReference (toName referenceIdentifier) referenceProps

    -- completions <- for imports $ \identifier -> do
    --   case identifier of
    --     (R1 (Py.DottedName _ (Py.Identifier ann name :| []))) -> do
    --       let referenceProps = Props.Reference ScopeGraph.Identifier ScopeGraph.Default (ann ^. span_ :: Span)
    --       complete <* newReference (Name.name name) referenceProps
    --     (L1 (Py.AliasedImport _ (Py.Identifier ann name) (Py.DottedName _ (Py.Identifier ann2 ref :| _)))) -> do
    --       let declProps = Props.Declaration ScopeGraph.UnqualifiedImport ScopeGraph.Default Nothing (ann ^. span_ :: Span)
    --       declare (Name.name name) declProps

    --       let referenceProps = Props.Reference ScopeGraph.Identifier ScopeGraph.Default (ann2 ^. span_ :: Span)
    --       newReference (Name.name ref) referenceProps

    --       complete
    --     (R1 (Py.DottedName _ ((Py.Identifier _ _) :| (_ : _)))) -> undefined

    -- pure (mconcat completions)
    todo ("Plz implement: ScopeGraph.hs l321" :: String)
  scopeGraph term = todo term

instance ToScopeGraph Py.Lambda where
  scopeGraph = todo

instance ToScopeGraph Py.List where
  scopeGraph = todo

instance ToScopeGraph Py.ListComprehension where
  scopeGraph = todo

instance ToScopeGraph Py.ListSplat where
  scopeGraph = todo

instance ToScopeGraph Py.NamedExpression where
  scopeGraph = todo

instance ToScopeGraph Py.None where
  scopeGraph = todo

instance ToScopeGraph Py.NonlocalStatement where
  scopeGraph = todo

instance ToScopeGraph Py.Module where
  scopeGraph _term@(Py.Module ann stmts) = do
    rootScope' <- rootScope

    -- TODO: Can probably be a newNodeWithName function
    moduleInfo <- ask @ModuleInfo
    let moduleName = Name.name . Text.pack . Path.toString . Path.takeBaseName $ Module.modulePath moduleInfo
    moduleScope <- scope moduleName
    CurrentScope currentScope' <- currentScope
    modify (Stack.addEdge moduleScope currentScope')
    putCurrentScope moduleScope

    declaration <- declare moduleName P.INTERFACE ann
    modify (Stack.addEdge rootScope' declaration)

    _ <- mapM (scopeGraph <=< ensureAST) stmts

    newGraph <- get @(Stack.Graph (Tagged Stack.Node))

    ScopeGraph.CurrentScope currentScope' <- currentScope
    modulePopSymbol <- popSymbol "."
    modify (Stack.addEdge declaration modulePopSymbol . Stack.addEdge modulePopSymbol currentScope' . Stack.overlay newGraph)

    pure (noBindings [])

instance ToScopeGraph Py.ReturnStatement where
  scopeGraph (Py.ReturnStatement _ maybeVals) = returnScopeGraph maybeVals

instance ToScopeGraph Py.True where
  scopeGraph = todo

instance ToScopeGraph Py.NotOperator where
  scopeGraph = todo

instance ToScopeGraph Py.Pair where
  scopeGraph = todo -- (Py.Pair _ value key) = scopeGraph key <> scopeGraph value

instance ToScopeGraph Py.ParenthesizedExpression where
  scopeGraph = todo

--onField @"extraChildren"

instance ToScopeGraph Py.PassStatement where
  scopeGraph _ = pure ([], [])

instance ToScopeGraph Py.PrintStatement where
  scopeGraph = todo

--(Py.PrintStatement _ args _chevron) = foldMap scopeGraph args

instance ToScopeGraph Py.PrimaryExpression where
  scopeGraph = todo

instance ToScopeGraph Py.SimpleStatement where
  scopeGraph (Py.SimpleStatement stmt) =
    scopeGraph stmt

instance ToScopeGraph Py.RaiseStatement where
  scopeGraph = todo

instance ToScopeGraph Py.Set where
  scopeGraph = todo

instance ToScopeGraph Py.SetComprehension where
  scopeGraph = todo

instance ToScopeGraph Py.String where
  scopeGraph = todo

instance ToScopeGraph Py.Subscript where
  scopeGraph = todo

instance ToScopeGraph Py.Tuple where
  scopeGraph = todo

instance ToScopeGraph Py.TryStatement where
  scopeGraph (Py.TryStatement _ eBody eClauses) = 
    childScopeGraph eBody <<>> aggregateChildScopeGraphs (toList eClauses)

instance ToScopeGraph Py.UnaryOperator where
  scopeGraph = todo

instance ToScopeGraph Py.WhileStatement where
  scopeGraph (Py.WhileStatement _ alternative body condition) = 
    childScopeGraph condition >> (childScopeGraph body <<>> optionalChildScopeGraph alternative ([],[]))

instance ToScopeGraph Py.WithStatement where
  scopeGraph = todo

instance ToScopeGraph Py.Yield where
  scopeGraph = todo

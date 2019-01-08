{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields, TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.TypeScript.Syntax.TypeScript where

import Prologue

import qualified Data.Text as T
import           Proto3.Suite

import           Control.Abstract hiding (Import)
import           Data.Abstract.Evaluatable as Evaluatable
import           Data.JSON.Fields
import qualified Data.Map.Strict as Map
import           Data.Semigroup.App
import           Data.Semigroup.Foldable
import           Data.Span
import           Diffing.Algorithm
import           Language.TypeScript.Resolution
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import Data.Aeson hiding (object)
import qualified Data.Abstract.Name as Name

data Import a = Import { importSymbols :: ![Alias], importFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

  -- http://www.typescriptlang.org/docs/handbook/module-resolution.html
instance Evaluatable Import where
  eval _ _ (Import symbols importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    ((moduleScope, moduleFrame), _) <- require modulePath
    if Prologue.null symbols then do
      insertImportEdge moduleScope
      insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    else do
      let scopeEdges = Map.singleton ScopeGraph.Import [ moduleScope ]
      scopeAddress <- newScope scopeEdges
      let frameLinks = Map.singleton ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
      frameAddress <- newFrame scopeAddress frameLinks

      -- Insert import references into the import scope starting from the perspective of the import scope.
      withScopeAndFrame moduleFrame $ do
        for_ symbols $ \Alias{..} ->
          -- TODO: Need an easier way to get the span of an Alias. It's difficult because we no longer have a term.
          -- Even if we had one we'd have to evaluate it at the moment.
          insertImportReference (Reference aliasName) emptySpan ScopeGraph.Identifier (Declaration aliasValue) scopeAddress

      -- Create edges from the current scope/frame to the import scope/frame.
      insertImportEdge scopeAddress
      insertFrameLink ScopeGraph.Import (Map.singleton scopeAddress frameAddress)
    pure unit

data QualifiedAliasedImport a = QualifiedAliasedImport { qualifiedAliasedImportAlias :: !a, qualifiedAliasedImportFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedAliasedImport where liftEq = genericLiftEq
instance Ord1 QualifiedAliasedImport where liftCompare = genericLiftCompare
instance Show1 QualifiedAliasedImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedAliasedImport where
  eval _ _ (QualifiedAliasedImport aliasTerm importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    ((moduleScope, moduleFrame), _) <- require modulePath
    span <- ask @Span

    importScope <- newScope (Map.singleton ScopeGraph.Import [ moduleScope ])
    let scopeMap = Map.singleton moduleScope moduleFrame
    aliasFrame <- newFrame importScope (Map.singleton ScopeGraph.Import scopeMap)

    alias <- maybeM (throwNoNameError aliasTerm) (declaredName aliasTerm)
    declare (Declaration alias) Default span ScopeGraph.QualifiedAliasedImport (Just importScope)
    aliasSlot <- lookupDeclaration (Declaration alias)
    assign aliasSlot =<< object aliasFrame

    pure unit


newtype SideEffectImport a = SideEffectImport { sideEffectImportFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 SideEffectImport where liftEq = genericLiftEq
instance Ord1 SideEffectImport where liftCompare = genericLiftCompare
instance Show1 SideEffectImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable SideEffectImport where
  eval _ _ (SideEffectImport importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    void $ require modulePath
    pure unit


-- | Qualified Export declarations
newtype QualifiedExport a = QualifiedExport { qualifiedExportSymbols :: [Alias] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedExport where liftEq = genericLiftEq
instance Ord1 QualifiedExport where liftCompare = genericLiftCompare
instance Show1 QualifiedExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedExport where
  eval _ _ (QualifiedExport exportSymbols) = do
    -- Create a Lexical edge from the qualifed export's scope to the current scope.
    currentScopeAddress <- currentScope
    let edges = Map.singleton Lexical [ currentScopeAddress ]
    exportScope <- newScope edges
    insertExportEdge exportScope -- Create an export edge from the current scope to the export scope
    withScope exportScope .
      for_ exportSymbols $ \Alias{..} -> do
        reference (Reference aliasName) emptySpan ScopeGraph.Identifier (Declaration aliasValue)

    -- Create an export edge from a new scope to the qualifed export's scope.
    pure unit

data Alias = Alias { aliasValue :: Name, aliasName :: Name }
  deriving (Eq, Generic, Hashable, Ord, Show, Message, Named, ToJSON, NFData)

toTuple :: Alias -> (Name, Name)
toTuple Alias{..} = (aliasValue, aliasName)

-- | Qualified Export declarations that export from another module.
data QualifiedExportFrom a = QualifiedExportFrom { qualifiedExportFrom :: ImportPath, qualifiedExportFromSymbols :: ![Alias]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedExportFrom where liftEq = genericLiftEq
instance Ord1 QualifiedExportFrom where liftCompare = genericLiftCompare
instance Show1 QualifiedExportFrom where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedExportFrom where
  eval _ _ (QualifiedExportFrom importPath exportSymbols) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions

    ((moduleScope, moduleFrame), _) <- require modulePath
    exportScope <- newScope (Map.singleton ScopeGraph.Import [ moduleScope ])
    exportFrame <- newFrame exportScope (Map.singleton ScopeGraph.Import (Map.singleton moduleScope moduleFrame))

    withScopeAndFrame moduleFrame .
      for_ exportSymbols $ \Alias{..} -> do
        insertImportReference (Reference aliasName) emptySpan ScopeGraph.Identifier (Declaration aliasValue) exportScope

    insertExportEdge exportScope
    insertFrameLink ScopeGraph.Export (Map.singleton exportScope exportFrame)

    pure unit

newtype DefaultExport a = DefaultExport { defaultExport :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 DefaultExport where liftEq = genericLiftEq
instance Ord1 DefaultExport where liftCompare = genericLiftCompare
instance Show1 DefaultExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DefaultExport where
  eval eval _ (DefaultExport term) = do
    case declaredName term of
      Just _ -> do
        exportScope <- newScope mempty
        exportFrame <- newFrame exportScope mempty
        exportSpan <- ask @Span
        withScopeAndFrame exportFrame $ do
          valueRef <- eval term
          let declaration = Declaration $ Name.name "__default"
          declare declaration Default exportSpan ScopeGraph.DefaultExport Nothing
          defaultSlot <- lookupDeclaration declaration
          assign defaultSlot valueRef

        insertExportEdge exportScope
        insertFrameLink ScopeGraph.Export (Map.singleton exportScope exportFrame)
      Nothing -> throwEvalError DefaultExportError
    pure unit


-- | Lookup type for a type-level key in a typescript map.
data LookupType a = LookupType { lookupTypeIdentifier :: a, lookupTypeKey :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 LookupType where liftEq = genericLiftEq
instance Ord1 LookupType where liftCompare = genericLiftCompare
instance Show1 LookupType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LookupType

-- | ShorthandPropertyIdentifier used in object patterns such as var baz = { foo } to mean var baz = { foo: foo }
newtype ShorthandPropertyIdentifier a = ShorthandPropertyIdentifier { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ShorthandPropertyIdentifier

instance Evaluatable ShorthandPropertyIdentifier

data Union a = Union { unionLeft :: !a, unionRight :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Union

instance Evaluatable Language.TypeScript.Syntax.TypeScript.Union

data Intersection a = Intersection { intersectionLeft :: !a, intersectionRight :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Intersection

instance Evaluatable Intersection

data AmbientFunction a = AmbientFunction { ambientFunctionContext :: ![a], ambientFunctionIdentifier :: !a, ambientFunctionParameters :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically AmbientFunction

instance Evaluatable AmbientFunction

newtype Tuple a = Tuple { tupleElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Tuple

-- This is a tuple type, not a tuple value, so we can't lean on the shared Tuple value
instance Evaluatable Tuple

data Constructor a = Constructor { constructorTypeParameters :: !a, constructorFormalParameters :: ![a], constructorType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Constructor

instance Evaluatable Language.TypeScript.Syntax.TypeScript.Constructor


newtype Annotation a = Annotation { annotationType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Annotation

instance Evaluatable Annotation

newtype Decorator a = Decorator { decoratorTerm :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Decorator

instance Evaluatable Decorator

newtype ComputedPropertyName a = ComputedPropertyName { propertyName :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ComputedPropertyName

instance Evaluatable ComputedPropertyName

newtype Constraint a = Constraint { constraintType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Constraint

instance Evaluatable Constraint

newtype DefaultType a = DefaultType { defaultType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 DefaultType where liftEq = genericLiftEq
instance Ord1 DefaultType where liftCompare = genericLiftCompare
instance Show1 DefaultType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable DefaultType

newtype ParenthesizedType a = ParenthesizedType { parenthesizedType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ParenthesizedType where liftEq = genericLiftEq
instance Ord1 ParenthesizedType where liftCompare = genericLiftCompare
instance Show1 ParenthesizedType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ParenthesizedType

newtype PredefinedType a = PredefinedType { predefinedType :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 PredefinedType where liftEq = genericLiftEq
instance Ord1 PredefinedType where liftCompare = genericLiftCompare
instance Show1 PredefinedType where liftShowsPrec = genericLiftShowsPrec
-- TODO: Implement Eval instance for PredefinedType
instance Evaluatable PredefinedType

newtype TypeIdentifier a = TypeIdentifier { contents :: T.Text }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Declarations1 TypeIdentifier where
  liftDeclaredName _ (TypeIdentifier identifier) = Just (Evaluatable.name identifier)

instance Eq1 TypeIdentifier where liftEq = genericLiftEq
instance Ord1 TypeIdentifier where liftCompare = genericLiftCompare
instance Show1 TypeIdentifier where liftShowsPrec = genericLiftShowsPrec
-- TODO: TypeIdentifier shouldn't evaluate to an address in the heap?
instance Evaluatable TypeIdentifier where
  eval _ _ TypeIdentifier{..} = do
    -- Add a reference to the type identifier in the current scope.
    span <- ask @Span
    reference (Reference (Evaluatable.name contents)) span ScopeGraph.TypeIdentifier (Declaration (Evaluatable.name contents))
    pure unit

data NestedIdentifier a = NestedIdentifier { left :: !a, right :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically NestedIdentifier

instance Evaluatable NestedIdentifier

newtype AmbientDeclaration a = AmbientDeclaration { ambientDeclarationBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically AmbientDeclaration

instance Evaluatable AmbientDeclaration where
  eval eval _ (AmbientDeclaration body) = eval body

data EnumDeclaration a = EnumDeclaration { enumDeclarationIdentifier :: !a, enumDeclarationBody :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically EnumDeclaration

instance Evaluatable EnumDeclaration

instance Declarations a => Declarations (EnumDeclaration a) where
  declaredName EnumDeclaration{..} = declaredName enumDeclarationIdentifier

newtype ExtendsClause a = ExtendsClause { extendsClauses :: [a] }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ExtendsClause

instance Declarations1 ExtendsClause where
  liftDeclaredName _ (ExtendsClause [])                 = Nothing
  liftDeclaredName declaredName (ExtendsClause (x : _)) = declaredName x

-- TODO: ExtendsClause shouldn't evaluate to an address in the heap?
instance Evaluatable ExtendsClause where
  eval eval _ ExtendsClause{..} = do
    -- Evaluate subterms
    traverse_ eval extendsClauses
    pure unit

data PropertySignature a = PropertySignature { modifiers :: ![a], propertySignaturePropertyName :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically PropertySignature

instance Evaluatable PropertySignature

data CallSignature a = CallSignature { callSignatureTypeParameters :: !a, callSignatureParameters :: ![a], callSignatureType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically CallSignature

instance Evaluatable CallSignature

-- | Todo: Move type params and type to context
data ConstructSignature a = ConstructSignature { constructSignatureTypeParameters :: !a, constructSignatureParameters :: ![a], constructSignatureType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ConstructSignature

instance Evaluatable ConstructSignature

data IndexSignature a = IndexSignature { subject :: a, subjectType :: a, typeAnnotation :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically IndexSignature

instance Evaluatable IndexSignature

data AbstractMethodSignature a = AbstractMethodSignature { abstractMethodSignatureContext :: ![a], abstractMethodSignatureName :: !a, abstractMethodSignatureParameters :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically AbstractMethodSignature

instance Evaluatable AbstractMethodSignature

data ForOf a = ForOf { forOfBinding :: !a, forOfSubject :: !a, forOfBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ForOf

instance Evaluatable ForOf

data LabeledStatement a = LabeledStatement { labeledStatementIdentifier :: !a, labeledStatementSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically LabeledStatement

instance Evaluatable LabeledStatement

newtype Update a = Update { updateSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Update

instance Evaluatable Update

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Module

declareModule :: ( AbstractValue term address value m
                 , Carrier sig m
                 , Declarations term
                 , Member (Allocator address) sig
                 , Member (Deref value) sig
                 , Member (Reader (CurrentFrame address)) sig
                 , Member (Reader (CurrentScope address)) sig
                 , Member (Reader Span) sig
                 , Member (Resumable (BaseError (EvalError term address value))) sig
                 , Member (State (Heap address address value)) sig
                 , Member (State (ScopeGraph address)) sig
                 , Member Fresh sig
                 , Member (Reader ModuleInfo) sig
                 , Member (Resumable (BaseError (AddressError address value))) sig
                 , Member (Resumable (BaseError (HeapError address))) sig
                 , Member (Resumable (BaseError (ScopeError address))) sig
                 , Ord address
                 )
                => (term -> Evaluator term address value m value)
                -> term
                -> [term]
                -> Evaluator term address value m value
declareModule eval identifier statements = do
    name <- maybeM (throwNoNameError identifier) (declaredName identifier)
    span <- ask @Span
    currentScope' <- currentScope

    let declaration = Declaration name
        moduleBody = maybe (pure unit) (runApp . foldMap1 (App . eval)) (nonEmpty statements)
    maybeSlot <- maybeLookupDeclaration declaration

    case maybeSlot of
      Just slot -> do
        moduleVal <- deref slot
        maybeFrame <- scopedEnvironment moduleVal
        case maybeFrame of
          Just moduleFrame -> do
            withScopeAndFrame moduleFrame moduleBody
          Nothing -> throwEvalError (DerefError moduleVal)
      Nothing -> do
        let edges = Map.singleton Lexical [ currentScope' ]
        childScope <- newScope edges
        declare (Declaration name) Default span ScopeGraph.Module (Just childScope)

        currentFrame' <- currentFrame
        let frameEdges = Map.singleton Lexical (Map.singleton currentScope' currentFrame')
        childFrame <- newFrame childScope frameEdges

        withScopeAndFrame childFrame (void moduleBody)

        moduleSlot <- lookupDeclaration (Declaration name)
        assign moduleSlot =<< namespace name childFrame

        pure unit

instance Evaluatable Module where
  eval eval _ Module{..} = declareModule eval moduleIdentifier moduleStatements

instance Declarations1 Module where
  liftDeclaredName declaredName = declaredName . moduleIdentifier

data InternalModule a = InternalModule { internalModuleIdentifier :: !a, internalModuleStatements :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically InternalModule

instance Evaluatable InternalModule where
  eval eval _ InternalModule{..} =
    declareModule eval internalModuleIdentifier internalModuleStatements

instance Declarations a => Declarations (InternalModule a) where
  declaredName InternalModule{..} = declaredName internalModuleIdentifier

data ClassHeritage a = ClassHeritage { classHeritageExtendsClause :: !a, implementsClause :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ClassHeritage

instance Evaluatable ClassHeritage

data AbstractClass a = AbstractClass { abstractClassIdentifier :: !a,  abstractClassTypeParameters :: !a, classHeritage :: ![a], classBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, NFData1, Named1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically AbstractClass

instance Declarations a => Declarations (AbstractClass a) where
  declaredName AbstractClass{..} = declaredName abstractClassIdentifier

instance Evaluatable AbstractClass where
  eval eval _ AbstractClass{..} = do
    name <- maybeM (throwNoNameError abstractClassIdentifier) (declaredName abstractClassIdentifier)
    span <- ask @Span
    currentScope' <- currentScope

    superScopes <- for classHeritage $ \superclass -> do
      name <- maybeM (throwNoNameError superclass) (declaredName superclass)
      scope <- associatedScope (Declaration name)
      slot <- lookupDeclaration (Declaration name)
      superclassFrame <- scopedEnvironment =<< deref slot
      pure $ case (scope, superclassFrame) of
        (Just scope, Just frame) -> Just (scope, frame)
        _                        -> Nothing

    let superclassEdges = (Superclass, ) . pure . fst <$> catMaybes superScopes
        current = (Lexical, ) <$> pure (pure currentScope')
        edges = Map.fromList (superclassEdges <> current)
    classScope <- newScope edges
    declare (Declaration name) Default span ScopeGraph.AbstractClass (Just classScope)

    let frameEdges = Map.singleton Superclass (Map.fromList (catMaybes superScopes))
    childFrame <- newFrame classScope frameEdges

    withScopeAndFrame childFrame $ do
      void $ eval classBody

    classSlot <- lookupDeclaration (Declaration name)
    assign classSlot =<< klass (Declaration name) childFrame

    pure unit

data MetaProperty a = MetaProperty
  deriving (Diffable, Eq, Foldable, Functor,  Generic1, Ord, Show, Traversable, FreeVariables1, Declarations1, ToJSONFields1, Hashable1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically MetaProperty

instance Evaluatable MetaProperty

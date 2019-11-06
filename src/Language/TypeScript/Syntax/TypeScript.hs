{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, FlexibleContexts, RecordWildCards, TupleSections, TypeApplications #-}
module Language.TypeScript.Syntax.TypeScript (module Language.TypeScript.Syntax.TypeScript) where

import Prologue

import           Control.Abstract hiding (Import)
import           Data.Abstract.Evaluatable as Evaluatable
import           Data.Abstract.ScopeGraph (AccessControl (..))
import           Data.JSON.Fields
import qualified Data.Map.Strict as Map
import           Data.Semigroup.App
import           Data.Semigroup.Foldable
import qualified Data.Text as T
import           Diffing.Algorithm
import qualified Data.Abstract.ScopeGraph as ScopeGraph

-- | ShorthandPropertyIdentifier used in object patterns such as var baz = { foo } to mean var baz = { foo: foo }
newtype ShorthandPropertyIdentifier a = ShorthandPropertyIdentifier { contents :: T.Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ShorthandPropertyIdentifier where liftEq = genericLiftEq
instance Ord1 ShorthandPropertyIdentifier where liftCompare = genericLiftCompare
instance Show1 ShorthandPropertyIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ShorthandPropertyIdentifier

data Union a = Union { unionLeft :: !a, unionRight :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Union where liftEq = genericLiftEq
instance Ord1 Union where liftCompare = genericLiftCompare
instance Show1 Union where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Language.TypeScript.Syntax.TypeScript.Union

data Intersection a = Intersection { intersectionLeft :: !a, intersectionRight :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Intersection where liftEq = genericLiftEq
instance Ord1 Intersection where liftCompare = genericLiftCompare
instance Show1 Intersection where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Intersection

data AmbientFunction a = AmbientFunction { ambientFunctionContext :: ![a], ambientFunctionIdentifier :: !a, ambientFunctionParameters :: ![a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 AmbientFunction where liftEq = genericLiftEq
instance Ord1 AmbientFunction where liftCompare = genericLiftCompare
instance Show1 AmbientFunction where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AmbientFunction

newtype Tuple a = Tuple { tupleElements :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

-- This is a tuple type, not a tuple value, so we can't lean on the shared Tuple value
instance Evaluatable Tuple

data Constructor a = Constructor { constructorTypeParameters :: !a, constructorFormalParameters :: ![a], constructorType :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Constructor where liftEq = genericLiftEq
instance Ord1 Constructor where liftCompare = genericLiftCompare
instance Show1 Constructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Language.TypeScript.Syntax.TypeScript.Constructor


newtype Annotation a = Annotation { annotationType :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Annotation

newtype Decorator a = Decorator { decoratorTerm :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Ord1 Decorator where liftCompare = genericLiftCompare
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Decorator

newtype ComputedPropertyName a = ComputedPropertyName { propertyName :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ComputedPropertyName where liftEq = genericLiftEq
instance Ord1 ComputedPropertyName where liftCompare = genericLiftCompare
instance Show1 ComputedPropertyName where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ComputedPropertyName

newtype Constraint a = Constraint { constraintType :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Constraint where liftEq = genericLiftEq
instance Ord1 Constraint where liftCompare = genericLiftCompare
instance Show1 Constraint where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Constraint

data NestedIdentifier a = NestedIdentifier { left :: !a, right :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 NestedIdentifier where liftEq = genericLiftEq
instance Ord1 NestedIdentifier where liftCompare = genericLiftCompare
instance Show1 NestedIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NestedIdentifier

newtype AmbientDeclaration a = AmbientDeclaration { ambientDeclarationBody :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 AmbientDeclaration where liftEq = genericLiftEq
instance Ord1 AmbientDeclaration where liftCompare = genericLiftCompare
instance Show1 AmbientDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AmbientDeclaration where
  eval eval _ (AmbientDeclaration body) = eval body

data EnumDeclaration a = EnumDeclaration { enumDeclarationIdentifier :: !a, enumDeclarationBody :: ![a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 EnumDeclaration where liftEq = genericLiftEq
instance Ord1 EnumDeclaration where liftCompare = genericLiftCompare
instance Show1 EnumDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable EnumDeclaration

instance Declarations a => Declarations (EnumDeclaration a) where
  declaredName EnumDeclaration{..} = declaredName enumDeclarationIdentifier

newtype ExtendsClause a = ExtendsClause { extendsClauses :: [a] }
  deriving (Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ExtendsClause where liftEq = genericLiftEq
instance Ord1 ExtendsClause where liftCompare = genericLiftCompare
instance Show1 ExtendsClause where liftShowsPrec = genericLiftShowsPrec

instance Declarations1 ExtendsClause where
  liftDeclaredName _ (ExtendsClause [])                 = Nothing
  liftDeclaredName declaredName (ExtendsClause (x : _)) = declaredName x

-- TODO: ExtendsClause shouldn't evaluate to an address in the heap?
instance Evaluatable ExtendsClause where
  eval eval _ ExtendsClause{..} = do
    -- Evaluate subterms
    traverse_ eval extendsClauses
    unit

data PropertySignature a = PropertySignature { modifiers :: [a], propertySignaturePropertyName :: a, propertySignatureAccessControl :: AccessControl }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 PropertySignature where liftEq = genericLiftEq
instance Ord1 PropertySignature where liftCompare = genericLiftCompare
instance Show1 PropertySignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PropertySignature

data CallSignature a = CallSignature { callSignatureTypeParameters :: !a, callSignatureParameters :: ![a], callSignatureType :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 CallSignature where liftEq = genericLiftEq
instance Ord1 CallSignature where liftCompare = genericLiftCompare
instance Show1 CallSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable CallSignature

-- | Todo: Move type params and type to context
data ConstructSignature a = ConstructSignature { constructSignatureTypeParameters :: !a, constructSignatureParameters :: ![a], constructSignatureType :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ConstructSignature where liftEq = genericLiftEq
instance Ord1 ConstructSignature where liftCompare = genericLiftCompare
instance Show1 ConstructSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ConstructSignature

data IndexSignature a = IndexSignature { subject :: a, subjectType :: a, typeAnnotation :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 IndexSignature where liftEq = genericLiftEq
instance Ord1 IndexSignature where liftCompare = genericLiftCompare
instance Show1 IndexSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable IndexSignature

data AbstractMethodSignature a = AbstractMethodSignature { abstractMethodSignatureContext :: ![a], abstractMethodSignatureName :: a, abstractMethodSignatureParameters :: [a], abstractMethodAccessControl :: AccessControl }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 AbstractMethodSignature where liftEq = genericLiftEq
instance Ord1 AbstractMethodSignature where liftCompare = genericLiftCompare
instance Show1 AbstractMethodSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AbstractMethodSignature

data ForOf a = ForOf { forOfBinding :: !a, forOfSubject :: !a, forOfBody :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ForOf where liftEq = genericLiftEq
instance Ord1 ForOf where liftCompare = genericLiftCompare
instance Show1 ForOf where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ForOf

data LabeledStatement a = LabeledStatement { labeledStatementIdentifier :: !a, labeledStatementSubject :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 LabeledStatement where liftEq = genericLiftEq
instance Ord1 LabeledStatement where liftCompare = genericLiftCompare
instance Show1 LabeledStatement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LabeledStatement

newtype Update a = Update { updateSubject :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Update where liftEq = genericLiftEq
instance Ord1 Update where liftCompare = genericLiftCompare
instance Show1 Update where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Update

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

declareModule :: ( AbstractValue term address value m
                 , Carrier sig m
                 , Declarations term
                 , Member (Allocator address) sig
                 , Member (Deref value) sig
                 , Member (Object address value) sig
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
                 , Member (Unit value) sig
                 , Ord address
                 )
                => (term -> Evaluator term address value m value)
                -> term
                -> [term]
                -> Evaluator term address value m value
declareModule eval identifier statements = do
    (name, relation) <- case declaredName identifier of
                          Just name -> pure (name, Default)
                          _         -> gensym >>= \name -> pure (name, Gensym)
    span <- ask @Span
    currentScope' <- currentScope

    let declaration = Declaration name
        moduleBody = maybe unit (runApp . foldMap1 (App . eval)) (nonEmpty statements)
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
        declare (Declaration name) relation Public span ScopeGraph.Module (Just childScope)

        currentFrame' <- currentFrame
        let frameEdges = Map.singleton Lexical (Map.singleton currentScope' currentFrame')
        childFrame <- newFrame childScope frameEdges

        withScopeAndFrame childFrame (void moduleBody)

        moduleSlot <- lookupSlot (Declaration name)
        assign moduleSlot =<< namespace name childFrame

        unit

instance Evaluatable Module where
  eval eval _ Module{..} = declareModule eval moduleIdentifier moduleStatements

instance Declarations1 Module where
  liftDeclaredName declaredName = declaredName . moduleIdentifier

data InternalModule a = InternalModule { internalModuleIdentifier :: !a, internalModuleStatements :: ![a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 InternalModule where liftEq = genericLiftEq
instance Ord1 InternalModule where liftCompare = genericLiftCompare
instance Show1 InternalModule where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InternalModule where
  eval eval _ InternalModule{..} =
    declareModule eval internalModuleIdentifier internalModuleStatements

instance Declarations a => Declarations (InternalModule a) where
  declaredName InternalModule{..} = declaredName internalModuleIdentifier

data ClassHeritage a = ClassHeritage { classHeritageExtendsClause :: !a, implementsClause :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ClassHeritage where liftEq = genericLiftEq
instance Ord1 ClassHeritage where liftCompare = genericLiftCompare
instance Show1 ClassHeritage where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ClassHeritage

data AbstractClass a = AbstractClass { abstractClassIdentifier :: !a,  abstractClassTypeParameters :: !a, classHeritage :: ![a], classBody :: !a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 AbstractClass where liftEq = genericLiftEq
instance Ord1 AbstractClass where liftCompare = genericLiftCompare
instance Show1 AbstractClass where liftShowsPrec = genericLiftShowsPrec

instance Declarations a => Declarations (AbstractClass a) where
  declaredName AbstractClass{..} = declaredName abstractClassIdentifier

instance Evaluatable AbstractClass where
  eval eval _ AbstractClass{..} = do
    span <- ask @Span
    currentScope' <- currentScope

    superScopes <- for classHeritage $ \superclass -> do
      name <- maybeM (throwNoNameError superclass) (declaredName superclass)
      scope <- associatedScope (Declaration name)
      slot <- lookupSlot (Declaration name)
      superclassFrame <- scopedEnvironment =<< deref slot
      pure $ case (scope, superclassFrame) of
        (Just scope, Just frame) -> Just (scope, frame)
        _                        -> Nothing

    let superclassEdges = (Superclass, ) . pure . fst <$> catMaybes superScopes
        current = (Lexical, ) <$> pure (pure currentScope')
        edges = Map.fromList (superclassEdges <> current)
    classScope <- newScope edges
    name <- declareMaybeName (declaredName abstractClassIdentifier) Default Public span ScopeGraph.AbstractClass (Just classScope)

    let frameEdges = Map.singleton Superclass (Map.fromList (catMaybes superScopes))
    childFrame <- newFrame classScope frameEdges

    withScopeAndFrame childFrame $ do
      void $ eval classBody

    classSlot <- lookupSlot (Declaration name)
    assign classSlot =<< klass (Declaration name) childFrame

    unit

data MetaProperty a = MetaProperty
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 MetaProperty where liftEq = genericLiftEq
instance Ord1 MetaProperty where liftCompare = genericLiftCompare
instance Show1 MetaProperty where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable MetaProperty

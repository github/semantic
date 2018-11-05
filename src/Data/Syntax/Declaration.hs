{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, ScopedTypeVariables, TupleSections, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.Syntax.Declaration where

import           Control.Abstract.ScopeGraph
import qualified Data.Abstract.Environment as Env
import           Data.Abstract.Evaluatable
import           Data.JSON.Fields
import qualified Data.Map.Strict as Map
import qualified Data.Reprinting.Scope as Scope
import qualified Data.Set as Set
import           Diffing.Algorithm
import           Prologue
import           Proto3.Suite.Class
import           Reprinting.Tokenize

data Function a = Function { functionContext :: ![a], functionName :: !a, functionParameters :: ![a], functionBody :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, ToJSONFields1, Named1, Message1, NFData1)

instance Diffable Function where
  equivalentBySubterm = Just . functionName

instance Eq1 Function where liftEq = genericLiftEq
instance Ord1 Function where liftCompare = genericLiftCompare
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

-- TODO: Filter the closed-over environment by the free variables in the term.
-- TODO: How should we represent function types, where applicable?

instance Evaluatable Function where
  eval _ Function{..} = do
    -- name <- maybeM (throwEvalError NoNameError) (declaredName functionName)
    -- (_, addr) <- letrec name (function (Just name) (paramNames functionParameters) functionBody)
    -- bind name addr
    -- pure (Rval addr)
    -- where paramNames = foldMap (maybeToList . declaredName)
    undefined

instance Tokenize Function where
  tokenize Function{..} = within' Scope.Function $ do
    functionName
    within' Scope.Params $ sequenceA_ (sep functionParameters)
    functionBody

instance Declarations1 Function where
  liftDeclaredName declaredName = declaredName . functionName

instance FreeVariables1 Function where
  liftFreeVariables freeVariables f@Function{..} = foldMap freeVariables f `Set.difference` foldMap freeVariables functionParameters


data Method a = Method { methodContext :: ![a], methodReceiver :: !a, methodName :: !a, methodParameters :: ![a], methodBody :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, ToJSONFields1, Named1, Message1, NFData1)

instance Eq1 Method where liftEq = genericLiftEq
instance Ord1 Method where liftCompare = genericLiftCompare
instance Show1 Method where liftShowsPrec = genericLiftShowsPrec

instance Diffable Method where
  equivalentBySubterm = Just . methodName

-- Evaluating a Method creates a closure and makes that value available in the
-- local environment.
instance Evaluatable Method where
  eval _ Method{..} = undefined
    -- name <- maybeM (throwEvalError NoNameError) (declaredName methodName)
    -- (_, addr) <- letrec name (function (Just name) (paramNames methodParameters) methodBody)
    -- bind name addr
    -- pure (Rval addr)
    -- where paramNames = foldMap (maybeToList . declaredName)

instance Tokenize Data.Syntax.Declaration.Method where
  tokenize Method{..} = within' Scope.Method $ do
    methodName
    within' Scope.Params $ sequenceA_ (sep methodParameters)
    methodBody

instance Declarations1 Method where
  liftDeclaredName declaredName = declaredName . methodName

instance FreeVariables1 Method where
  liftFreeVariables freeVariables m@Method{..} = foldMap freeVariables m `Set.difference` foldMap freeVariables methodParameters


-- | A method signature in TypeScript or a method spec in Go.
data MethodSignature a = MethodSignature { methodSignatureContext :: ![a], methodSignatureName :: !a, methodSignatureParameters :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 MethodSignature where liftEq = genericLiftEq
instance Ord1 MethodSignature where liftCompare = genericLiftCompare
instance Show1 MethodSignature where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for MethodSignature
instance Evaluatable MethodSignature


newtype RequiredParameter a = RequiredParameter { requiredParameter :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 RequiredParameter where liftEq = genericLiftEq
instance Ord1 RequiredParameter where liftCompare = genericLiftCompare
instance Show1 RequiredParameter where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for RequiredParameter
instance Evaluatable RequiredParameter


newtype OptionalParameter a = OptionalParameter { optionalParameter :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 OptionalParameter where liftEq = genericLiftEq
instance Ord1 OptionalParameter where liftCompare = genericLiftCompare
instance Show1 OptionalParameter where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for OptionalParameter
instance Evaluatable OptionalParameter


-- TODO: Should we replace this with Function and differentiate by context?
-- TODO: How should we distinguish class/instance methods?
-- TODO: It would be really nice to have a more meaningful type contained in here than [a]
-- | A declaration of possibly many variables such as var foo = 5, bar = 6 in JavaScript.
newtype VariableDeclaration a = VariableDeclaration { variableDeclarations :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 VariableDeclaration where liftEq = genericLiftEq
instance Ord1 VariableDeclaration where liftCompare = genericLiftCompare
instance Show1 VariableDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable VariableDeclaration where
  eval _ (VariableDeclaration [])   = rvalBox unit
  eval eval (VariableDeclaration decs) = do
    addresses <- for decs $ \declaration -> do
      name <- maybeM (throwEvalError NoNameError) (declaredName declaration)
      (span, valueRef) <- do
        ref <- eval declaration
        subtermSpan <- get @Span
        pure (subtermSpan, ref)

      declare (Declaration name) span Nothing -- TODO is it true that variable declarations never have an associated scope?
    rvalBox unit

instance Declarations a => Declarations (VariableDeclaration a) where
  declaredName (VariableDeclaration vars) = case vars of
    [var] -> declaredName var
    _     -> Nothing


-- | A TypeScript/Java style interface declaration to implement.

data InterfaceDeclaration a = InterfaceDeclaration { interfaceDeclarationContext :: ![a], interfaceDeclarationIdentifier :: !a, interfaceDeclarationSuperInterfaces :: ![a], interfaceDeclarationBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 InterfaceDeclaration where liftEq = genericLiftEq
instance Ord1 InterfaceDeclaration where liftCompare = genericLiftCompare
instance Show1 InterfaceDeclaration where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for InterfaceDeclaration
instance Evaluatable InterfaceDeclaration

instance Declarations a => Declarations (InterfaceDeclaration a) where
  declaredName InterfaceDeclaration{..} = declaredName interfaceDeclarationIdentifier


-- | A public field definition such as a field definition in a JavaScript class.
data PublicFieldDefinition a = PublicFieldDefinition { publicFieldContext :: ![a], publicFieldPropertyName :: !a, publicFieldValue :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 PublicFieldDefinition where liftEq = genericLiftEq
instance Ord1 PublicFieldDefinition where liftCompare = genericLiftCompare
instance Show1 PublicFieldDefinition where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for PublicFieldDefinition
instance Evaluatable PublicFieldDefinition where
  eval _ PublicFieldDefinition{..} = do
    span <- ask @Span
    propertyName <- maybeM (throwEvalError NoNameError) (declaredName publicFieldPropertyName)
    declare (Declaration propertyName) span Nothing
    rvalBox unit



data Variable a = Variable { variableName :: !a, variableType :: !a, variableValue :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Variable where liftEq = genericLiftEq
instance Ord1 Variable where liftCompare = genericLiftCompare
instance Show1 Variable where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Variable
instance Evaluatable Variable

data Class a = Class { classContext :: ![a], classIdentifier :: !a, classSuperclasses :: ![a], classBody :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1, NFData1)

instance Declarations a => Declarations (Class a) where
  declaredName (Class _ name _ _) = declaredName name

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Class where
  eval eval Class{..} = do
    name <- maybeM (throwEvalError NoNameError) (declaredName classIdentifier)
    span <- ask @Span
    -- Run the action within the class's scope.
    currentScope' <- currentScope

    supers <- for classSuperclasses $ \superclass -> do
      name <- maybeM (throwEvalError NoNameError) (declaredName superclass)
      scope <- associatedScope (Declaration name)
      undefined
    undefined
      --   (scope,) <$> (eval superclass >>= address)
      --
      -- let imports = (Import,) <$> (fmap pure . catMaybes $ fst <$> supers)
      --     current = maybe mempty (fmap (Lexical, ) . pure . pure) currentScope'
      --     edges = Map.fromList (imports <> current)
      -- childScope <- newScope edges
      -- declare (Declaration name) span (Just childScope)
      --
      -- withScope childScope $ do
      --   (_, addr) <- letrec name $ do
      --     void $ eval classBody
      --     classBinds <- Env.head <$> getEnv
      --     klass name (snd <$> supers) classBinds
      --   bind name addr
      --   pure (Rval addr)

-- | A decorator in Python
data Decorator a = Decorator { decoratorIdentifier :: !a, decoratorParamaters :: ![a], decoratorBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Ord1 Decorator where liftCompare = genericLiftCompare
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Decorator
instance Evaluatable Decorator

-- TODO: Generics, constraints.


-- | An ADT, i.e. a disjoint sum of products, like 'data' in Haskell, or 'enum' in Rust or Swift.
data Datatype a = Datatype { datatypeContext :: a, datatypeName :: a, datatypeConstructors :: [a], datatypeDeriving :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Data.Syntax.Declaration.Datatype where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Declaration.Datatype where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Declaration.Datatype where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Datatype
instance Evaluatable Data.Syntax.Declaration.Datatype


-- | A single constructor in a datatype, or equally a 'struct' in C, Rust, or Swift.
data Constructor a = Constructor { constructorContext :: [a], constructorName :: a, constructorFields :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Data.Syntax.Declaration.Constructor where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Declaration.Constructor where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Declaration.Constructor where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Constructor
instance Evaluatable Data.Syntax.Declaration.Constructor


-- | Comprehension (e.g. ((a for b in c if a()) in Python)
data Comprehension a = Comprehension { comprehensionValue :: !a, comprehensionBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Comprehension where liftEq = genericLiftEq
instance Ord1 Comprehension where liftCompare = genericLiftCompare
instance Show1 Comprehension where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Comprehension
instance Evaluatable Comprehension


-- | A declared type (e.g. `a []int` in Go).
data Type a = Type { typeName :: !a, typeKind :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Type where liftEq = genericLiftEq
instance Ord1 Type where liftCompare = genericLiftCompare
instance Show1 Type where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Type
instance Evaluatable Type


-- | Type alias declarations in Javascript/Haskell, etc.
data TypeAlias a = TypeAlias { typeAliasContext :: ![a], typeAliasIdentifier :: !a, typeAliasKind :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeAlias where liftEq = genericLiftEq
instance Ord1 TypeAlias where liftCompare = genericLiftCompare
instance Show1 TypeAlias where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeAlias where
  eval eval TypeAlias{..} = do
    -- name <- maybeM (throwEvalError NoNameError) (declaredName typeAliasIdentifier)
    -- addr <- eval typeAliasKind >>= address
    -- bind name addr
    -- pure (Rval addr)

    -- name <- maybeM (throwEvalError NoNameError) (declaredName (subterm typeAliasIdentifier))
    -- kindName <- maybeM (throwEvalError NoNameError) (declaredName (subterm typeAliasKind))
    -- span <- ask @Span
    -- address <- declare (Declaration name) span Nothing -- TODO: Also need to declare the alias via an Alias edge?
    -- kindAddress <- lookupDeclaration (Declaration kindName) -- TODO: Validate the path? Also assumes the type is declared.
    -- rvalBox unit -- TODO: Return Address here?
    undefined

instance Declarations a => Declarations (TypeAlias a) where
  declaredName TypeAlias{..} = declaredName typeAliasIdentifier

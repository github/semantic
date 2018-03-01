{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}
module Data.Syntax.Declaration where

import Analysis.Abstract.Evaluating
import Control.Monad.Effect.Addressable
import Control.Monad.Effect.Evaluatable
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Value
import Diffing.Algorithm
import Prelude hiding (fail)
import Prologue
import qualified Data.Abstract.Type as Type
import qualified Data.Abstract.Value as Value

data Function a = Function { functionContext :: ![a], functionName :: !a, functionParameters :: ![a], functionBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Diffable Function where
  equivalentBySubterm = Just . functionName

instance Eq1 Function where liftEq = genericLiftEq
instance Ord1 Function where liftCompare = genericLiftCompare
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement evaluation under the binder for the typechecking evaluator.
-- TODO: Filter the closed-over environment by the free variables in the term.
-- TODO: How should we represent function types, where applicable?

instance ( FreeVariables t
         , Semigroup (Cell l (Value l t))
         , Addressable l es
         , Member (State (EnvironmentFor (Value l t))) es
         , Member (Reader (EnvironmentFor (Value l t))) es
         , Member (State (StoreFor (Value l t))) es
         ) => Evaluatable es t (Value l t) Function where
  eval Function{..} = do
    env <- ask
    let params = toList (liftFreeVariables (freeVariables . subterm) functionParameters)
    -- FIXME: Can we store the action evaluating the body in the Value instead of the body term itself?
    let v = inj (Closure params (subterm functionBody) env) :: Value l t

    (name, addr) <- lookupOrAlloc (subterm functionName) v env
    modify (envInsert name addr)
    pure v

-- TODO: Re-implement type checking with 'Evaluatable' approach.
instance Member Fail es => Evaluatable es t Type.Type Function
-- instance ( Alternative m
--          , Monad m
--          , MonadFresh m
--          , MonadEnv Type.Type m
--          , MonadStore Type.Type m
--          , FreeVariables t
--          )
--          => Eval t Type.Type m Function where
--   eval recur yield Function{..} = do
--     env <- askEnv @Type.Type
--     let params = toList (foldMap freeVariables functionParameters)
--     tvars <- for params $ \name -> do
--       a <- alloc name
--       tvar <- Var <$> fresh
--       assign a tvar
--       pure (name, a, tvar)
--
--     outTy <- localEnv (const (foldr (\ (n, a, _) -> envInsert n a) env tvars)) (recur pure functionBody)
--     let tvars' = fmap (\(_, _, t) -> t) tvars
--     let v = Type.Product tvars' :-> outTy
--
--     (name, a) <- lookupOrAlloc functionName env v
--
--     localEnv (envInsert name a) (yield v)


data Method a = Method { methodContext :: ![a], methodReceiver :: !a, methodName :: !a, methodParameters :: ![a], methodBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Diffable Method where
  equivalentBySubterm = Just . methodName

instance Eq1 Method where liftEq = genericLiftEq
instance Ord1 Method where liftCompare = genericLiftCompare
instance Show1 Method where liftShowsPrec = genericLiftShowsPrec

-- Evaluating a Method creates a closure and makes that value available in the
-- local environment.
instance ( FreeVariables t                -- To get free variables from the function's parameters
         , Semigroup (Cell l (Value l t)) -- lookupOrAlloc
         , Addressable l es              -- lookupOrAlloc
         , Member (State (EnvironmentFor (Value l t))) es
         , Member (Reader (EnvironmentFor (Value l t))) es
         , Member (State (StoreFor (Value l t))) es
         ) => Evaluatable es t (Value l t) Method where
  eval Method{..} = do
    env <- ask
    let params = toList (liftFreeVariables (freeVariables . subterm) methodParameters)
    -- FIXME: Can we store the action evaluating the body in the Value instead of the body term itself?
    let v = inj (Closure params (subterm methodBody) env) :: Value l t

    (name, addr) <- lookupOrAlloc (subterm methodName) v env
    modify (envInsert name addr)
    pure v

-- TODO: Implement Evaluatable instance for type checking
instance Member Fail es => Evaluatable es t Type.Type Method

-- | A method signature in TypeScript or a method spec in Go.
data MethodSignature a = MethodSignature { _methodSignatureContext :: ![a], _methodSignatureName :: !a, _methodSignatureParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 MethodSignature where liftEq = genericLiftEq
instance Ord1 MethodSignature where liftCompare = genericLiftCompare
instance Show1 MethodSignature where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for MethodSignature
instance Member Fail es => Evaluatable es t v MethodSignature


newtype RequiredParameter a = RequiredParameter { requiredParameter :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 RequiredParameter where liftEq = genericLiftEq
instance Ord1 RequiredParameter where liftCompare = genericLiftCompare
instance Show1 RequiredParameter where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for RequiredParameter
instance Member Fail es => Evaluatable es t v RequiredParameter


newtype OptionalParameter a = OptionalParameter { optionalParameter :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 OptionalParameter where liftEq = genericLiftEq
instance Ord1 OptionalParameter where liftCompare = genericLiftCompare
instance Show1 OptionalParameter where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for OptionalParameter
instance Member Fail es => Evaluatable es t v OptionalParameter


-- TODO: Should we replace this with Function and differentiate by context?
-- TODO: How should we distinguish class/instance methods?

-- | A declaration of possibly many variables such as var foo = 5, bar = 6 in JavaScript.
newtype VariableDeclaration a = VariableDeclaration { variableDeclarations :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 VariableDeclaration where liftEq = genericLiftEq
instance Ord1 VariableDeclaration where liftCompare = genericLiftCompare
instance Show1 VariableDeclaration where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for VariableDeclaration
instance Member Fail es => Evaluatable es t v VariableDeclaration


-- | A TypeScript/Java style interface declaration to implement.
data InterfaceDeclaration a = InterfaceDeclaration { interfaceDeclarationContext :: ![a], interfaceDeclarationIdentifier :: !a, interfaceDeclarationBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 InterfaceDeclaration where liftEq = genericLiftEq
instance Ord1 InterfaceDeclaration where liftCompare = genericLiftCompare
instance Show1 InterfaceDeclaration where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for InterfaceDeclaration
instance Member Fail es => Evaluatable es t v InterfaceDeclaration


-- | A public field definition such as a field definition in a JavaScript class.
data PublicFieldDefinition a = PublicFieldDefinition { publicFieldContext :: ![a], publicFieldPropertyName :: !a, publicFieldValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 PublicFieldDefinition where liftEq = genericLiftEq
instance Ord1 PublicFieldDefinition where liftCompare = genericLiftCompare
instance Show1 PublicFieldDefinition where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for PublicFieldDefinition
instance Member Fail es => Evaluatable es t v PublicFieldDefinition


data Variable a = Variable { variableName :: !a, variableType :: !a, variableValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Variable where liftEq = genericLiftEq
instance Ord1 Variable where liftCompare = genericLiftCompare
instance Show1 Variable where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Variable
instance Member Fail es => Evaluatable es t v Variable

data Class a = Class { classContext :: ![a], classIdentifier :: !a, classSuperclasses :: ![a], classBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Class
instance Member Fail es => Evaluatable es t v Class


data Module a = Module { moduleIdentifier :: !a, moduleScope :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Module
instance Member Fail es => Evaluatable es t v Module

-- | A decorator in Python
data Decorator a = Decorator { decoratorIdentifier :: !a, decoratorParamaters :: ![a], decoratorBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Ord1 Decorator where liftCompare = genericLiftCompare
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Decorator
instance Member Fail es => Evaluatable es t v Decorator

-- TODO: Generics, constraints.


-- | An ADT, i.e. a disjoint sum of products, like 'data' in Haskell, or 'enum' in Rust or Swift.
data Datatype a = Datatype { datatypeName :: !a, datatypeConstructors :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Data.Syntax.Declaration.Datatype where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Declaration.Datatype where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Declaration.Datatype where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Datatype
instance Member Fail es => Evaluatable es t v Data.Syntax.Declaration.Datatype


-- | A single constructor in a datatype, or equally a 'struct' in C, Rust, or Swift.
data Constructor a = Constructor { constructorName :: !a, constructorFields :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Data.Syntax.Declaration.Constructor where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Declaration.Constructor where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Declaration.Constructor where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Constructor
instance Member Fail es => Evaluatable es t v Data.Syntax.Declaration.Constructor


-- | Comprehension (e.g. ((a for b in c if a()) in Python)
data Comprehension a = Comprehension { comprehensionValue :: !a, comprehensionBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Comprehension where liftEq = genericLiftEq
instance Ord1 Comprehension where liftCompare = genericLiftCompare
instance Show1 Comprehension where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Comprehension
instance Member Fail es => Evaluatable es t v Comprehension

-- | Import declarations.
data Import a = Import { importFrom :: !a, importAlias :: !a, importSymbols :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

instance ( Show l
         , Show t
         , Semigroup (Cell l (Value l t)) -- lookupOrAlloc
         , Members (Evaluating (Value l t)) es
         , Addressable l es
         , Evaluatable es t (Value l t) (Base t)
         , Recursive t
         , FreeVariables t
         -- , HasField fields Range
         )
         => Evaluatable es t (Value l t) Import where
  eval (Import from alias _) = do
    -- Capture current global environment
    env <- get @(EnvironmentFor (Value l t))

    -- Evaluate the import, the interface value we get back will contain an
    -- environment but evaluating will have also have potentially updated the
    -- global environment.
    interface <- require @(Value l t) (qualifiedName (subterm from))

    -- Restore previous global environment, adding the imported env
    (name, addr) <- lookupOrAlloc' (qualifiedName (subterm alias)) interface env
    modify (const (envInsert name addr env)) -- const is important—we are throwing away the modified global env and specifically crafting a new environment.

    pure interface

instance Member Fail es => Evaluatable es t Type.Type Import


-- | An imported symbol
data ImportSymbol a = ImportSymbol { importSymbolName :: !a, importSymbolAlias :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ImportSymbol where liftEq = genericLiftEq
instance Ord1 ImportSymbol where liftCompare = genericLiftCompare
instance Show1 ImportSymbol where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ImportSymbol
instance Member Fail es => Evaluatable es t v ImportSymbol


-- | A declared type (e.g. `a []int` in Go).
data Type a = Type { typeName :: !a, typeKind :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Type where liftEq = genericLiftEq
instance Ord1 Type where liftCompare = genericLiftCompare
instance Show1 Type where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Type
instance Member Fail es => Evaluatable es t v Type


-- | Type alias declarations in Javascript/Haskell, etc.
data TypeAlias a = TypeAlias { typeAliasContext :: ![a], typeAliasIdentifier :: !a, typeAliasKind :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TypeAlias where liftEq = genericLiftEq
instance Ord1 TypeAlias where liftCompare = genericLiftCompare
instance Show1 TypeAlias where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeAlias
instance Member Fail es => Evaluatable es t v TypeAlias

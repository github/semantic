{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}
module Data.Syntax.Declaration where

import Control.Applicative
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Embedded
import Control.Monad.Effect.State
import Control.Monad.Effect.Address
import Control.Monad.Effect.Env
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Store
import Control.Monad.Effect.Store2 (Store2)
import Control.Monad.Effect (Eff)
import Control.Monad.Effect.Fail
import Data.Abstract.Address
import Data.Abstract.Environment
import Analysis.Abstract.Evaluating
import qualified Analysis.Abstract.Evaluating3 as E3
import Data.Abstract.Eval
import qualified Data.Abstract.Eval2 as E2
import qualified Data.Abstract.Eval3 as E3
import Data.Abstract.FreeVariables
import Data.Abstract.Type hiding (Type)
import qualified Data.Abstract.Value as Value
import qualified Data.Abstract.Type as Type
import qualified Data.ByteString.Char8 as BC
import Data.Abstract.Value
import Data.Align.Generic
import Data.Foldable (toList)
import Data.Functor.Classes.Generic
import Data.Mergeable
import Data.Traversable
import Data.Semigroup
import Data.Union
import Diffing.Algorithm
import GHC.Generics
import Prelude hiding (fail)

data Function a = Function { functionContext :: ![a], functionName :: !a, functionParameters :: ![a], functionBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Diffable Function where
  equivalentBySubterm = Just . functionName

instance Eq1 Function where liftEq = genericLiftEq
instance Ord1 Function where liftCompare = genericLiftCompare
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

-- TODO: Do we need some distinct notion of a global environment?
-- TODO: Implement evaluation under the binder for the typechecking evaluator.
-- TODO: Filter the closed-over environment by the free variables in the term.
instance ( Monad m
         , Ord l
         , Semigroup (Cell l (Value l t))
         , MonadEnv (Value l t) m
         , MonadStore (Value l t) m
         , MonadAddress l m
         , FreeVariables t
         ) => Eval t (Value l t) m Function where
  eval _ yield Function{..} = do
    env <- askEnv @(Value l t)
    let params = toList (foldMap freeVariables functionParameters)
    let v = inj (Closure params functionBody env)

    (name, a) <- envLookupOrAlloc' functionName env v

    localEnv (envInsert name a) (yield v)

instance ( Alternative m
         , Monad m
         , MonadFresh m
         , MonadEnv Type.Type m
         , MonadStore Type.Type m
         , FreeVariables t
         )
         => Eval t Type.Type m Function where
  eval recur yield Function{..} = do
    env <- askEnv @Type.Type
    let params = toList (foldMap freeVariables functionParameters)
    tvars <- for params $ \name -> do
      a <- alloc name
      tvar <- Var <$> fresh
      assign a tvar
      pure (name, a, tvar)

    outTy <- localEnv (const (foldr (\ (n, a, _) -> envInsert n a) env tvars)) (recur pure functionBody)
    let tvars' = fmap (\(_, _, t) -> t) tvars
    let v = Type.Product tvars' :-> outTy

    (name, a) <- envLookupOrAlloc' functionName env v

    localEnv (envInsert name a) (yield v)

instance ( FreeVariables t
         , Semigroup (Cell l (Value l t))
         , MonadStore (Value l t) (Eff es)
         , MonadAddress l (Eff es)
         , Member (State (E3.EnvironmentFor (Value l t))) es
         , Member (Reader (E3.EnvironmentFor (Value l t))) es
         ) => E3.Evaluatable es t (Value l t) Function where
  eval Function{..} = do
    env <- ask @(E3.EnvironmentFor (Value l t))
    let params = toList (freeVariables1 functionParameters)
    let v = inj (Closure params functionBody env) :: Value l t

    (name, addr) <- envLookupOrAlloc' functionName env v
    modify (envInsert name addr)
    pure v

instance ( Member Fail es ) => E3.Evaluatable es t Type.Type Function

-- TODO: How should we represent function types, where applicable?

data Method a = Method { methodContext :: ![a], methodReceiver :: !a, methodName :: !a, methodParameters :: ![a], methodBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Diffable Method where
  equivalentBySubterm = Just . methodName

instance Eq1 Method where liftEq = genericLiftEq
instance Ord1 Method where liftCompare = genericLiftCompare
instance Show1 Method where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Method
instance (MonadFail m) => Eval t v m Method

-- Evaluating a Function creates a closure and makes that value available in the
-- local environment.
instance ( Monad m
         , FreeVariables t                -- To get free variables from the function's parameters
         , Semigroup (Cell l (Value l t)) -- envLookupOrAlloc'
         , MonadStore (Value l t) m       -- envLookupOrAlloc'
         , MonadAddress l m               -- envLookupOrAlloc'
         , E2.EvalEnv (Value l t) m         -- 'yield'
         ) => E2.Eval t (Value l t) m Method where
  eval Method{..} = do
    env <- E2.askEnv @(Value l t)
    let params = toList (freeVariables1 methodParameters)
    let v = inj (Closure params methodBody env) :: Value l t

    (name, addr) <- envLookupOrAlloc' methodName env v
    E2.modifyEnv (envInsert name addr)
    pure v

instance MonadFail m => E2.Eval t Type.Type m Method

instance ( FreeVariables t                -- To get free variables from the function's parameters
         , Semigroup (Cell l (Value l t)) -- envLookupOrAlloc'
         , MonadStore (Value l t) (Eff es)       -- envLookupOrAlloc'
         , MonadAddress l (Eff es)               -- envLookupOrAlloc'
         , Member (State (E3.EnvironmentFor (Value l t))) es
         , Member (Reader (E3.EnvironmentFor (Value l t))) es
         ) => E3.Evaluatable es t (Value l t) Method where
  eval Method{..} = do
    env <- ask @(E3.EnvironmentFor (Value l t))
    let params = toList (freeVariables1 methodParameters)
    let v = inj (Closure params methodBody env) :: Value l t

    (name, addr) <- envLookupOrAlloc' methodName env v
    modify (envInsert name addr)
    pure v

instance ( Member Fail es ) => E3.Evaluatable es t Type.Type Method

-- | A method signature in TypeScript or a method spec in Go.
data MethodSignature a = MethodSignature { _methodSignatureContext :: ![a], _methodSignatureName :: !a, _methodSignatureParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 MethodSignature where liftEq = genericLiftEq
instance Ord1 MethodSignature where liftCompare = genericLiftCompare
instance Show1 MethodSignature where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for MethodSignature
instance (MonadFail m) => Eval t v m MethodSignature


data RequiredParameter a = RequiredParameter { requiredParameter :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 RequiredParameter where liftEq = genericLiftEq
instance Ord1 RequiredParameter where liftCompare = genericLiftCompare
instance Show1 RequiredParameter where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for RequiredParameter
instance (MonadFail m) => Eval t v m RequiredParameter


data OptionalParameter a = OptionalParameter { optionalParameter :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 OptionalParameter where liftEq = genericLiftEq
instance Ord1 OptionalParameter where liftCompare = genericLiftCompare
instance Show1 OptionalParameter where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for OptionalParameter
instance (MonadFail m) => Eval t v m OptionalParameter


-- TODO: Should we replace this with Function and differentiate by context?
-- TODO: How should we distinguish class/instance methods?

-- | A declaration of possibly many variables such as var foo = 5, bar = 6 in JavaScript.
newtype VariableDeclaration a = VariableDeclaration { variableDeclarations :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 VariableDeclaration where liftEq = genericLiftEq
instance Ord1 VariableDeclaration where liftCompare = genericLiftCompare
instance Show1 VariableDeclaration where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for VariableDeclaration
instance (MonadFail m) => Eval t v m VariableDeclaration


-- | A TypeScript/Java style interface declaration to implement.
data InterfaceDeclaration a = InterfaceDeclaration { interfaceDeclarationContext :: ![a], interfaceDeclarationIdentifier :: !a, interfaceDeclarationBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 InterfaceDeclaration where liftEq = genericLiftEq
instance Ord1 InterfaceDeclaration where liftCompare = genericLiftCompare
instance Show1 InterfaceDeclaration where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for InterfaceDeclaration
instance (MonadFail m) => Eval t v m InterfaceDeclaration


-- | A public field definition such as a field definition in a JavaScript class.
data PublicFieldDefinition a = PublicFieldDefinition { publicFieldContext :: ![a], publicFieldPropertyName :: !a, publicFieldValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 PublicFieldDefinition where liftEq = genericLiftEq
instance Ord1 PublicFieldDefinition where liftCompare = genericLiftCompare
instance Show1 PublicFieldDefinition where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for PublicFieldDefinition
instance (MonadFail m) => Eval t v m PublicFieldDefinition


data Variable a = Variable { variableName :: !a, variableType :: !a, variableValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Variable where liftEq = genericLiftEq
instance Ord1 Variable where liftCompare = genericLiftCompare
instance Show1 Variable where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Variable
instance (MonadFail m) => Eval t v m Variable

data Class a = Class { classContext :: ![a], classIdentifier :: !a, classSuperclasses :: ![a], classBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Class
instance (MonadFail m) => Eval t v m Class


data Module a = Module { moduleIdentifier :: !a, moduleScope :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Module
instance (MonadFail m) => Eval t v m Module

-- | A decorator in Python
data Decorator a = Decorator { decoratorIdentifier :: !a, decoratorParamaters :: ![a], decoratorBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Ord1 Decorator where liftCompare = genericLiftCompare
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Decorator
instance (MonadFail m) => Eval t v m Decorator

-- TODO: Generics, constraints.


-- | An ADT, i.e. a disjoint sum of products, like 'data' in Haskell, or 'enum' in Rust or Swift.
data Datatype a = Datatype { datatypeName :: !a, datatypeConstructors :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Data.Syntax.Declaration.Datatype where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Declaration.Datatype where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Declaration.Datatype where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Datatype
instance (MonadFail m) => Eval t v m Data.Syntax.Declaration.Datatype


-- | A single constructor in a datatype, or equally a 'struct' in C, Rust, or Swift.
data Constructor a = Constructor { constructorName :: !a, constructorFields :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Data.Syntax.Declaration.Constructor where liftEq = genericLiftEq
instance Ord1 Data.Syntax.Declaration.Constructor where liftCompare = genericLiftCompare
instance Show1 Data.Syntax.Declaration.Constructor where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Constructor
instance (MonadFail m) => Eval t v m Data.Syntax.Declaration.Constructor


-- | Comprehension (e.g. ((a for b in c if a()) in Python)
data Comprehension a = Comprehension { comprehensionValue :: !a, comprehensionBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Comprehension where liftEq = genericLiftEq
instance Ord1 Comprehension where liftCompare = genericLiftCompare
instance Show1 Comprehension where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Comprehension
instance (MonadFail m) => Eval t v m Comprehension

-- | Import declarations.
data Import a = Import { importFrom :: !a, importAlias :: !a, importSymbols :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

instance ( Monad m
         , Show l
         , Show t
         , MonadFail m
         , MonadLinker (Value l t) m
         , MonadEnv (Value l t) m
         , FreeVariables t
         )
         => Eval t (Value l t) m Import where
  eval _ yield (Import from _ _) = do
    let [name] = toList (freeVariables from)

    interface <- require (BC.unpack name)
    Interface _ env <- maybe
                           (fail ("expected an interface, but got: " <> show interface))
                           pure
                           (prj interface :: Maybe (Value.Interface l t))

    localEnv (envUnion env) (yield interface)

instance MonadFail m => Eval t Type.Type m Import

instance (MonadFail m) => E2.Eval t v m Import


instance ( Show l
         , Show t
         , Members (E3.Evaluating (Value l t)) es
         , FreeVariables t
         )
         => E3.Evaluatable es t (Value l t) Import where
  eval (Import from _ _) = do
    let [name] = toList (freeVariables from)

    interface <- E3.require (BC.unpack name)
    -- TODO: Consider returning the value instead of the interface.
    Interface _ env <- maybe
                           (fail ("expected an interface, but got: " <> show interface))
                           pure
                           (prj interface :: Maybe (Value.Interface l t))

    modify (envUnion env)
    pure interface
--
instance Member Fail es => E3.Evaluatable es t Type.Type Import


-- | An imported symbol
data ImportSymbol a = ImportSymbol { importSymbolName :: !a, importSymbolAlias :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 ImportSymbol where liftEq = genericLiftEq
instance Ord1 ImportSymbol where liftCompare = genericLiftCompare
instance Show1 ImportSymbol where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ImportSymbol
instance (MonadFail m) => Eval t v m ImportSymbol


-- | A declared type (e.g. `a []int` in Go).
data Type a = Type { typeName :: !a, typeKind :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Type where liftEq = genericLiftEq
instance Ord1 Type where liftCompare = genericLiftCompare
instance Show1 Type where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Type
instance (MonadFail m) => Eval t v m Type


-- | Type alias declarations in Javascript/Haskell, etc.
data TypeAlias a = TypeAlias { typeAliasContext :: ![a], typeAliasIdentifier :: !a, typeAliasKind :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TypeAlias where liftEq = genericLiftEq
instance Ord1 TypeAlias where liftCompare = genericLiftCompare
instance Show1 TypeAlias where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeAlias
instance (MonadFail m) => Eval t v m TypeAlias

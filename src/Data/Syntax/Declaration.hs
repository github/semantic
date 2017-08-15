{-# LANGUAGE DeriveAnyClass #-}
module Data.Syntax.Declaration where

import Algorithm
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics

data Function a = Function { functionName :: !a, functionParameters :: ![a], functionBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Function where liftEq = genericLiftEq
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

-- TODO: How should we represent function types, where applicable?

data Method a = Method { methodReceiver :: !a, methodName :: !a, methodParameters :: ![a], methodBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Method where liftEq = genericLiftEq
instance Show1 Method where liftShowsPrec = genericLiftShowsPrec

-- TODO: Should we replace this with Function and differentiate by context?
-- TODO: How should we distinguish class/instance methods?

data Variable a = Variable { variableName :: !a, variableType :: !a, variableValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Variable where liftEq = genericLiftEq
instance Show1 Variable where liftShowsPrec = genericLiftShowsPrec


data Class a = Class { classIdentifier :: !a, classSuperclasses :: ![a], classBody :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Class where liftEq = genericLiftEq
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec


data Module a = Module { moduleIdentifier :: !a, moduleScope :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Module where liftEq = genericLiftEq
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec


-- | A decorator in Python
data Decorator a = Decorator { decoratorIdentifier :: !a, decoratorParamaters :: ![a], decoratorBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec

-- TODO: Generics, constraints.


-- | An ADT, i.e. a disjoint sum of products, like 'data' in Haskell, or 'enum' in Rust or Swift.
data Datatype a = Datatype { datatypeName :: !a, datatypeConstructors :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Data.Syntax.Declaration.Datatype where liftEq = genericLiftEq
instance Show1 Data.Syntax.Declaration.Datatype where liftShowsPrec = genericLiftShowsPrec

-- | A single constructor in a datatype, or equally a 'struct' in C, Rust, or Swift.
data Constructor a = Constructor { constructorName :: !a, constructorFields :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Data.Syntax.Declaration.Constructor where liftEq = genericLiftEq
instance Show1 Data.Syntax.Declaration.Constructor where liftShowsPrec = genericLiftShowsPrec


-- | Comprehension (e.g. ((a for b in c if a()) in Python)
data Comprehension a = Comprehension { comprehensionValue :: !a, comprehensionBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Comprehension where liftEq = genericLiftEq
instance Show1 Comprehension where liftShowsPrec = genericLiftShowsPrec


-- | Import declarations.
data Import a = Import { importContent :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Import where liftEq = genericLiftEq
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

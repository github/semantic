{-# LANGUAGE DeriveAnyClass #-}
module Data.Syntax.Declaration where

import Algorithm
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Pretty.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics

data Function a = Function { functionName :: !a, functionParameters :: ![a], functionBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Function where liftEq = genericLiftEq
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Function where liftPretty = genericLiftPretty

-- TODO: How should we represent function types, where applicable?

data Method a = Method { methodReceiver :: !a, methodName :: !a, methodParameters :: ![a], methodBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Method where liftEq = genericLiftEq
instance Show1 Method where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Method where liftPretty = genericLiftPretty

-- TODO: Should we replace this with Function and differentiate by context?
-- TODO: How should we distinguish class/instance methods?

data Variable a = Variable { variableName :: !a, variableType :: !a, variableValue :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Variable where liftEq = genericLiftEq
instance Show1 Variable where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Variable where liftPretty = genericLiftPretty


data Class a = Class { classIdentifier :: !a, classSuperclasses :: ![a], classBody :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Class where liftEq = genericLiftEq
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Class where liftPretty = genericLiftPretty


data Module a = Module { moduleIdentifier :: !a, moduleScope :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Module where liftEq = genericLiftEq
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Module where liftPretty = genericLiftPretty

data Interface a = Interface { interfaceIdentifier :: !a, interfaceBody :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Interface where liftEq = genericLiftEq
instance Show1 Interface where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Interface where liftPretty = genericLiftPretty

-- | A decorator in Python
data Decorator a = Decorator { decoratorIdentifier :: !a, decoratorParamaters :: ![a], decoratorBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Decorator where liftPretty = genericLiftPretty

-- TODO: Generics, constraints.


-- | An ADT, i.e. a disjoint sum of products, like 'data' in Haskell, or 'enum' in Rust or Swift.
data Datatype a = Datatype { datatypeName :: !a, datatypeConstructors :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Data.Syntax.Declaration.Datatype where liftEq = genericLiftEq
instance Show1 Data.Syntax.Declaration.Datatype where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Data.Syntax.Declaration.Datatype where liftPretty = genericLiftPretty

-- | A single constructor in a datatype, or equally a 'struct' in C, Rust, or Swift.
data Constructor a = Constructor { constructorName :: !a, constructorFields :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Data.Syntax.Declaration.Constructor where liftEq = genericLiftEq
instance Show1 Data.Syntax.Declaration.Constructor where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Data.Syntax.Declaration.Constructor where liftPretty = genericLiftPretty


-- | Comprehension (e.g. ((a for b in c if a()) in Python)
data Comprehension a = Comprehension { comprehensionValue :: !a, comprehensionBody :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Comprehension where liftEq = genericLiftEq
instance Show1 Comprehension where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Comprehension where liftPretty = genericLiftPretty


-- | Import declarations.
data Import a = Import { importContent :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Import where liftEq = genericLiftEq
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Import where liftPretty = genericLiftPretty

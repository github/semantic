module Data.Syntax.Declaration where

import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics
import Prologue

data Function a = Function { functionName :: !a, functionParameters :: ![a], functionBody :: !a }
  deriving (Eq, Foldable, Generic1, Show)

instance Eq1 Function where liftEq = genericLiftEq
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

-- TODO: How should we represent function types, where applicable?

data Method a = Method { methodName :: !a, methodParameters :: ![a], methodBody :: !a }
  deriving (Eq, Foldable, Generic1, Show)

instance Eq1 Method where liftEq = genericLiftEq
instance Show1 Method where liftShowsPrec = genericLiftShowsPrec

-- TODO: Should we replace this with Function and differentiate by context?
-- TODO: How should we distinguish class/instance methods?


data Class a = Class { classIdentifier :: !a, classSuperclasses :: ![a], classScope :: ![a] }
  deriving (Eq, Foldable, Generic1, Show)

instance Eq1 Class where liftEq = genericLiftEq
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

-- TODO: Generics, constraints.


-- | An ADT, i.e. a disjoint sum of products, like 'data' in Haskell, or 'enum' in Rust or Swift.
data Datatype a = Datatype { datatypeName :: !a, datatypeConstructors :: ![a] }
  deriving (Eq, Foldable, Generic1, Show)

instance Eq1 Data.Syntax.Declaration.Datatype where liftEq = genericLiftEq
instance Show1 Data.Syntax.Declaration.Datatype where liftShowsPrec = genericLiftShowsPrec

-- | A single constructor in a datatype, or equally a 'struct' in C, Rust, or Swift.
data Constructor a = Constructor { constructorName :: !a, constructorFields :: ![a] }
  deriving (Eq, Foldable, Generic1, Show)

instance Eq1 Data.Syntax.Declaration.Constructor where liftEq = genericLiftEq
instance Show1 Data.Syntax.Declaration.Constructor where liftShowsPrec = genericLiftShowsPrec

module Data.Syntax.Declaration where

import Prologue

data Function a = Function { functionName :: !a, functionParameters :: ![a], functionBody :: !a }
  deriving (Eq, Show)

-- TODO: How should we represent function types, where applicable?

data Method a = Method { methodName :: !a, methodParameters :: ![a], methodBody :: !a }
  deriving (Eq, Show)

-- TODO: Should we replace this with Function and differentiate by context?
-- TODO: How should we distinguish class/instance methods?


data Class a = Class { classIdentifier :: !a, classSuperclasses :: ![a], classScope :: !a }
  deriving (Eq, Show)

-- TODO: Generics, constraints.


-- | An ADT, i.e. a disjoint sum of products, like 'data' in Haskell, or 'enum' in Rust or Swift.
data Datatype a = Datatype { datatypeName :: !a, datatypeConstructors :: ![a] }
  deriving (Eq, Show)

-- | A single constructor in a datatype.
data Constructor a = Constructor { constructorName :: !a, constructorFields :: ![a] }
  deriving (Eq, Show)

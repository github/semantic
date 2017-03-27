module Data.Syntax.Declaration where

import Prologue

data Function a = Function { functionName :: !a, functionParameters :: ![a], functionBody :: !a }
  deriving (Eq, Show)

-- TODO: How should we represent function types, where applicable?

data Method a = Method { methodName :: !a, methodParameters :: ![a], methodBody :: !a }
  deriving (Eq, Show)

-- TODO: Should we replace this with Function and differentiate by context?
-- TODO: How should we distinguish class/instance methods?

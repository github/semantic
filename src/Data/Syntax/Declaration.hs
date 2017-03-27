module Data.Syntax.Declaration where

import Prologue

data Function a = Function { functionName :: !a, functionParameters :: ![a], functionBody :: !a }
  deriving (Eq, Show)

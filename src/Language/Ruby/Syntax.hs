{-# LANGUAGE DeriveAnyClass #-}
module Language.Ruby.Syntax where

import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Evaluatable
import Diffing.Algorithm
-- import Prelude hiding (fail)
import Prologue
import qualified Data.Map as Map

data Require a = Require { requireRelative :: Bool, requirePath :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Require where liftEq = genericLiftEq
instance Ord1 Require where liftCompare = genericLiftCompare
instance Show1 Require where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Require where
  eval (Require _ x) = do
    name <- pathToQualifiedName <$> (subtermValue x >>= asString)
    importedEnv <- isolate (require name)
    modifyGlobalEnv (flip (Map.foldrWithKey envInsert) (unEnvironment importedEnv))
    unit

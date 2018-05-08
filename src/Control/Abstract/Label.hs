{-# LANGUAGE GADTs, TypeOperators #-}
module Control.Abstract.Label
( JumpTable
, Instruction
, Label
, label
, goto
, Goto(..)
) where

import           Control.Abstract.Context
import           Control.Abstract.Evaluator
import qualified Data.IntMap as IntMap
import           Prelude hiding (fail)
import           Prologue

type JumpTable term = IntMap.IntMap (Instruction term)
type Instruction term = (PackageInfo, ModuleInfo, term)

-- | The type of labels.
--   TODO: This should be rolled into 'Name' and tracked in the environment, both so that we can abstract over labels like any other location, and so that we can garbage collect unreachable labels.
type Label = Int


getJumpTable :: Member (State (JumpTable term)) effects => Evaluator location term vlaue effects (JumpTable term)
getJumpTable = raise get

-- | Allocate a 'Label' for the given @term@.
--
--   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
label :: Members '[Reader ModuleInfo, Reader PackageInfo, State (JumpTable term)] effects => term -> Evaluator location term value effects Label
label term = do
  m <- getJumpTable
  moduleInfo <- currentModule
  packageInfo <- currentPackage
  let i = IntMap.size m
  raise (put (IntMap.insert i (packageInfo, moduleInfo, term) m))
  pure i

-- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
goto :: Members '[Fail, Reader ModuleInfo, Reader PackageInfo, State (JumpTable term)] effects => Label -> (term -> Evaluator location term value effects a) -> Evaluator location term value effects a
goto label comp = do
  maybeTerm <- IntMap.lookup label <$> getJumpTable
  case maybeTerm of
    Just (packageInfo, moduleInfo, term) -> raiseHandler (local (const packageInfo)) (raiseHandler (local (const moduleInfo)) (comp term))
    Nothing -> raise (fail ("unknown label: " <> show label))


data Goto effects value return where
  Label :: PackageInfo -> ModuleInfo -> Eff (Goto effects value ': effects) value -> Goto effects value Label
  Goto  :: Label -> Goto effects value (PackageInfo, ModuleInfo, Eff (Goto effects value ': effects) value)

{-# LANGUAGE GADTs, TypeOperators #-}
module Control.Abstract.Goto
( JumpTable
, Instruction
, Label
, label
, goto
, Goto(..)
, runGoto
) where

import           Control.Abstract.Context
import           Control.Abstract.Evaluator
import           Control.Monad.Effect (Eff, relayState)
import qualified Data.IntMap as IntMap
import           Prelude hiding (fail)
import           Prologue

type JumpTable effects value = IntMap.IntMap (Instruction effects value)
type Instruction effects value = (PackageInfo, ModuleInfo, Eff effects value)

-- | The type of labels.
--   TODO: This should be rolled into 'Name' and tracked in the environment, both so that we can abstract over labels like any other location, and so that we can garbage collect unreachable labels.
type Label = Int


-- | Allocate a 'Label' for the given @term@.
--
--   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
label :: Members '[Reader ModuleInfo, Reader PackageInfo] effects => Evaluator location term value (Goto effects value ': effects) value -> Evaluator location term value (Goto effects value ': effects) Label
label action = do
  moduleInfo <- currentModule
  packageInfo <- currentPackage
  send (Label packageInfo moduleInfo (lower action))

-- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
goto :: Members '[Reader ModuleInfo, Reader PackageInfo] effects => Label -> (Evaluator location term value (Goto effects value ': effects) value -> Evaluator location term value (Goto effects value ': effects) a) -> Evaluator location term value (Goto effects value ': effects) a
goto label comp = do
  (packageInfo, moduleInfo, action) <- send (Goto label)
  raiseHandler (local (const packageInfo)) (raiseHandler (local (const moduleInfo)) (comp (raise action)))


data Goto effects value return where
  Label :: PackageInfo -> ModuleInfo -> Eff (Goto effects value ': effects) value -> Goto effects value Label
  Goto  :: Label -> Goto effects value (PackageInfo, ModuleInfo, Eff (Goto effects value ': effects) value)

runGoto :: Member Fail effects => JumpTable (Goto effects value ': effects) value -> Evaluator location term value (Goto effects value ': effects) a -> Evaluator location term value effects a
runGoto initial = raiseHandler (relayState (IntMap.size initial, initial) (const pure) (\ (supremum, table) goto yield -> case goto of
  Label packageInfo moduleInfo action -> yield (succ supremum, IntMap.insert supremum (packageInfo, moduleInfo, action) table) supremum
  Goto label                          -> case IntMap.lookup label table of
    Just (packageInfo, moduleInfo, action) -> yield (supremum, table) (packageInfo, moduleInfo, action)
    Nothing                                -> raise (fail ("unknown label: " <> show label))))

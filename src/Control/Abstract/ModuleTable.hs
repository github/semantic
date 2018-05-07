module Control.Abstract.ModuleTable
( ModuleTable
, getModuleTable
, putModuleTable
, modifyModuleTable
, askModuleTable
, askLoadStack
, localLoadStack
, resolve
, listModulesInDir
) where

import Control.Abstract.Evaluator
import Data.Abstract.Environment
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Prologue

-- | Retrieve the table of evaluated modules.
getModuleTable :: Member (State (ModuleTable (Environment location value, value))) effects => Evaluator location term value effects (ModuleTable (Environment location value, value))
getModuleTable = raise get

-- | Set the table of evaluated modules.
putModuleTable :: Member (State (ModuleTable (Environment location value, value))) effects => ModuleTable (Environment location value, value) -> Evaluator location term value effects ()
putModuleTable = raise . put

-- | Update the evaluated module table.
modifyModuleTable :: Member (State (ModuleTable (Environment location value, value))) effects => (ModuleTable (Environment location value, value) -> ModuleTable (Environment location value, value)) -> Evaluator location term value effects ()
modifyModuleTable = raise . modify'


-- | Retrieve the table of unevaluated modules.
askModuleTable :: Member (Reader (ModuleTable [Module term])) effects
               => Evaluator location term value effects (ModuleTable [Module term])
askModuleTable = raise ask


-- | Retrieve the module load stack
askLoadStack :: Member (Reader LoadStack) effects => Evaluator location term value effects LoadStack
askLoadStack = raise ask

-- | Locally update the module load stack.
localLoadStack :: Member (Reader LoadStack) effects => (LoadStack -> LoadStack) -> Evaluator location term value effects a -> Evaluator location term value effects a
localLoadStack = raiseHandler . local


-- Resolve a list of module paths to a possible module table entry.
resolve :: Member (Reader (ModuleTable [Module term])) effects
        => [FilePath]
        -> Evaluator location term value effects (Maybe ModulePath)
resolve names = do
  tbl <- askModuleTable
  pure $ find (`ModuleTable.member` tbl) names

listModulesInDir :: Member (Reader (ModuleTable [Module term])) effects
                 => FilePath
                 -> Evaluator location term value effects [ModulePath]
listModulesInDir dir = modulePathsInDir dir <$> askModuleTable

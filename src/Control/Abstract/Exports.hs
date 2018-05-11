module Control.Abstract.Exports
( Exports
, getExports
, putExports
, modifyExports
, addExport
, withExports
) where

import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.Exports
import Data.Abstract.FreeVariables

-- | Get the global export state.
getExports :: Member (State (Exports location value)) effects => Evaluator location value effects (Exports location value)
getExports = get

-- | Set the global export state.
putExports :: Member (State (Exports location value)) effects => Exports location value -> Evaluator location value effects ()
putExports = put

-- | Update the global export state.
modifyExports :: Member (State (Exports location value)) effects => (Exports location value -> Exports location value) -> Evaluator location value effects ()
modifyExports = modify'

-- | Add an export to the global export state.
addExport :: Member (State (Exports location value)) effects => Name -> Name -> Maybe (Address location value) -> Evaluator location value effects ()
addExport name alias = modifyExports . insert name alias

-- | Sets the global export state for the lifetime of the given action.
withExports :: Member (State (Exports location value)) effects => Exports location value -> Evaluator location value effects a -> Evaluator location value effects a
withExports = localState . const

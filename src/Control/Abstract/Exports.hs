module Control.Abstract.Exports
( Exports
, getExports
, putExports
, modifyExports
, addExport
, withExports
) where

import Control.Abstract.Evaluator
import Data.Abstract.Exports
import Data.Abstract.Name

-- | Get the global export state.
getExports :: Member (State (Exports address)) effects => Evaluator address value effects (Exports address)
getExports = get

-- | Set the global export state.
putExports :: Member (State (Exports address)) effects => Exports address -> Evaluator address value effects ()
putExports = put

-- | Update the global export state.
modifyExports :: Member (State (Exports address)) effects => (Exports address -> Exports address) -> Evaluator address value effects ()
modifyExports = modify'

-- | Add an export to the global export state.
addExport :: Member (State (Exports address)) effects => Name -> Name -> Maybe address -> Evaluator address value effects ()
addExport name alias = modifyExports . insert name alias

-- | Sets the global export state for the lifetime of the given action.
withExports :: Member (State (Exports address)) effects => Exports address -> Evaluator address value effects a -> Evaluator address value effects a
withExports = localState . const

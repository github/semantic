module Control.Abstract.Exports
( Exports
, getExports
, addExport
) where

import Control.Abstract.Evaluator
import Data.Abstract.Exports
import Data.Abstract.Name

-- | Get the global export state.
getExports :: Member (State (Exports address)) effects => Evaluator address value effects (Exports address)
getExports = get

-- | Add an export to the global export state.
addExport :: Member (State (Exports address)) effects => Name -> Name -> Maybe address -> Evaluator address value effects ()
addExport name alias = modify' . insert name alias

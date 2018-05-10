{-# LANGUAGE GADTs, TypeOperators #-}
module Control.Abstract.Goto
( GotoTable
, Label
, label
, goto
, Goto(..)
, runGoto
) where

import           Control.Abstract.Evaluator
import           Control.Monad.Effect (Eff)
import qualified Data.IntMap as IntMap
import           Prelude hiding (fail)
import           Prologue

type GotoTable inner value = IntMap.IntMap (Eff (Goto inner value ': inner) value)

-- | The type of labels.
--   TODO: This should be rolled into 'Name' and tracked in the environment, both so that we can abstract over labels like any other location, and so that we can garbage collect unreachable labels.
type Label = Int


-- | Allocate a 'Label' for the given @term@.
--
--   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
label :: Evaluator location value (Goto effects value ': effects) value -> Evaluator location value (Goto effects value ': effects) Label
label = send . Label . lower

-- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated.
goto :: Label -> Evaluator location value (Goto effects value ': effects) (Evaluator location value (Goto effects value ': effects) value)
goto = fmap raise . send . Goto


-- | 'Goto' effects embed an 'Eff' action which can be run in the environment under the 'Goto' itself.
--
--   It’s tempting to try to use a 'Member' constraint to require a 'Goto' effect:
--
--   @
--   foo :: Member (Goto effects a) effects => Eff effects a
--   @
--
--   However, using this type would require that the type of the effect list include a reference to itself, which is forbidden by the occurs check: we wouldn’t be able to write a handler for 'Goto' if it could be used at that type. Instead, one can either use a smaller, statically known effect list inside the 'Goto', e.g. @Member (Goto outer) inner@ where @outer@ is a suffix of @inner@ (and with some massaging to raise the @outer@ actions into the @inner@ context), or use 'Goto' when it’s statically known to be the head of the list: @Eff (Goto rest a ': rest) b@. In either case, the 'Eff' actions embedded in the effect are themselves able to contain further 'Goto' effects,
data Goto effects value return where
  Label :: Eff (Goto effects value ': effects) value -> Goto effects value Label
  Goto  :: Label -> Goto effects value (Eff (Goto effects value ': effects) value)

runGoto :: Members '[ Fail
                    , Fresh
                    , State table
                    ] effects
        => (GotoTable effects value -> table)
        -> (table -> GotoTable effects value)
        -> Evaluator location value (Goto effects value ': effects) a
        -> Evaluator location value effects a
runGoto from to = runEffect (\ goto yield -> do
  table <- to <$> getTable
  case goto of
    Label action -> do
      supremum <- raise fresh
      putTable (from (IntMap.insert supremum action table))
      yield supremum
    Goto label   -> maybe (raise (fail ("unknown label: " <> show label))) yield (IntMap.lookup label table))

getTable :: Member (State table) effects => Evaluator location value effects table
getTable = raise get

putTable :: Member (State table) effects => table -> Evaluator location value effects ()
putTable = raise . put

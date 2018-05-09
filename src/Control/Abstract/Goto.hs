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
import           Control.Monad.Effect (Eff, relayState)
import qualified Data.IntMap as IntMap
import           Prelude hiding (fail)
import           Prologue

type GotoTable effects value = IntMap.IntMap (Eff effects value)

-- | The type of labels.
--   TODO: This should be rolled into 'Name' and tracked in the environment, both so that we can abstract over labels like any other location, and so that we can garbage collect unreachable labels.
type Label = Int


-- | Allocate a 'Label' for the given @term@.
--
--   Labels must be allocated before being jumped to with 'goto', but are suitable for nonlocal jumps; thus, they can be used to implement coroutines, exception handling, call with current continuation, and other esoteric control mechanisms.
label :: Evaluator location value (Goto effects value ': effects) value -> Evaluator location value (Goto effects value ': effects) Label
label = send . Label . lower

-- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated in e.g. a 'MonadAnalysis' instance).
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

runGoto :: Member Fail effects => GotoTable (Goto effects value ': effects) value -> Evaluator location value (Goto effects value ': effects) a -> Evaluator location value effects (a, GotoTable (Goto effects value ': effects) value)
runGoto initial = raiseHandler (relayState (IntMap.size initial, initial) (\ (_, table) a -> pure (a, table)) (\ (supremum, table) goto yield -> case goto of
  Label action -> yield (succ supremum, IntMap.insert supremum action table) supremum
  Goto label   -> maybe (fail ("unknown label: " <> show label)) (yield (supremum, table)) (IntMap.lookup label table)))

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
label = send . Label . lowerEff

-- | “Jump” to a previously-allocated 'Label' (retrieving the @term@ at which it points, which can then be evaluated.
goto :: Label -> Evaluator location value (Goto effects value ': effects) (Evaluator location value (Goto effects value ': effects) value)
goto = fmap raiseEff . send . Goto


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

-- | Run a 'Goto' effect in terms of a 'State' effect holding a 'GotoTable', accessed via wrap/unwrap functions.
--
--   The wrap/unwrap functions are necessary in order for ghc to be able to typecheck the table, since it necessarily contains references to its own effect list. Since @GotoTable (… ': State (GotoTable … value) ': …) value@ can’t be written, and a recursive type equality constraint won’t typecheck, callers will need to employ a @newtype@ to break the self-reference. The effect list of the table the @newtype@ contains will include all of the effects between the 'Goto' effect and the 'State' effect (including the 'State' but not the 'Goto'). E.g. if the 'State' is the next effect, a valid wrapper would be∷
--
--   @
--   newtype Gotos effects value = Gotos { getGotos :: GotoTable (State (Gotos effects value) ': effects) value }
--   @
--
--   Callers can then evaluate the high-level 'Goto' effect by passing @Gotos@ and @getGotos@ to 'runGoto'.
runGoto :: Members '[ Fail
                    , Fresh
                    , State table
                    ] effects
        => (GotoTable effects value -> table)
        -> (table -> GotoTable effects value)
        -> Evaluator location value (Goto effects value ': effects) a
        -> Evaluator location value effects a
runGoto from to = interpret (\ goto -> do
  table <- to <$> getTable
  case goto of
    Label action -> do
      supremum <- fresh
      supremum <$ putTable (from (IntMap.insert supremum action table))
    Goto label   -> maybeM (raiseEff (fail ("unknown label: " <> show label))) (IntMap.lookup label table))

getTable :: Member (State table) effects => Evaluator location value effects table
getTable = get

putTable :: Member (State table) effects => table -> Evaluator location value effects ()
putTable = put

module Control.Abstract.Primitive where

import Control.Abstract.Addressable
import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.Value
import Data.Abstract.Name
import Data.Semigroup.Reducer hiding (unit)
import Data.Semilattice.Lower
import Data.Text (pack, unpack)
import Prologue

builtin :: ( HasCallStack
           , Member (Allocator address value) effects
           , Member (Reader ModuleInfo) effects
           , Member (Reader Span) effects
           , Member (State (Environment address)) effects
           , Member (State (Heap address (Cell address) value)) effects
           , Ord address
           , Reducer value (Cell address value)
           )
        => String
        -> Evaluator address value effects value
        -> Evaluator address value effects ()
builtin s def = withCurrentCallStack callStack $ do
  let name' = name ("__semantic_" <> T.pack s)
  addr <- alloc name'
  bind name' addr
  def >>= assign addr

lambda :: (AbstractFunction address value effects, Member Fresh effects)
       => (Name -> Evaluator address value effects value)
       -> Evaluator address value effects value
lambda body = do
  var <- nameI <$> fresh
  closure [var] lowerBound (body var)

defineBuiltins :: ( AbstractValue address value effects
                  , HasCallStack
                  , Member (Allocator address value) effects
                  , Member Fresh effects
                  , Member (Reader (Environment address)) effects
                  , Member (Reader ModuleInfo) effects
                  , Member (Reader Span) effects
                  , Member (Resumable (EnvironmentError address)) effects
                  , Member (State (Environment address)) effects
                  , Member (State (Heap address (Cell address) value)) effects
                  , Member Trace effects
                  , Ord address
                  , Reducer value (Cell address value)
                  )
               => Evaluator address value effects ()
defineBuiltins =
  builtin "print" (lambda (\ v -> variable v >>= asString >>= trace . T.unpack >> pure unit))

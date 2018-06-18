module Control.Abstract.Primitive where

import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.Value
import qualified Data.Abstract.Environment as Env
import Data.Abstract.Name
import Data.Text (unpack)
import Prologue

define :: ( HasCallStack
          , Member (Allocator address value) effects
          , Member (Env address) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader Span) effects
          )
       => Name
       -> Evaluator address value effects value
       -> Evaluator address value effects ()
define name def = withCurrentCallStack callStack $ do
  addr <- alloc name
  bind name addr
  def >>= assign addr

defineClass :: ( AbstractValue address value effects
               , HasCallStack
               , Member (Allocator address value) effects
               , Member (Env address) effects
               , Member (Reader ModuleInfo) effects
               , Member (Reader Span) effects
               )
            => Name
            -> [Name]
            -> Evaluator address value effects a
            -> Evaluator address value effects ()
defineClass name superclasses scope = define name $ do
  env <- locally $ do
    void scope
    Env.head <$> getEnv
  klass name (map (string . formatName) superclasses) env

lambda :: (AbstractFunction address value effects, Member Fresh effects)
       => (Name -> Evaluator address value effects address)
       -> Evaluator address value effects value
lambda body = do
  var <- nameI <$> fresh
  closure [var] lowerBound (body var)

defineBuiltins :: ( AbstractValue address value effects
                  , HasCallStack
                  , Member (Allocator address value) effects
                  , Member (Env address) effects
                  , Member Fresh effects
                  , Member (Reader ModuleInfo) effects
                  , Member (Reader Span) effects
                  , Member (Resumable (EnvironmentError address)) effects
                  , Member Trace effects
                  )
               => Evaluator address value effects ()
defineBuiltins =
  define "__semantic_print" (lambda (\ v -> variable v >>= deref >>= asString >>= trace . unpack >> box unit))

{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
module Control.Abstract.Primitive where

import Control.Abstract.Addressable
import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.Value
import Data.Abstract.Environment
import Data.Abstract.Name
import Data.ByteString.Char8 (pack, unpack)
import Data.Char
import Data.Semigroup.Reducer hiding (unit)
import Data.Semilattice.Lower
import Prologue

data Builtin args result where
  Print :: Builtin String ()

deriving instance Eq   (Builtin args result)
deriving instance Ord  (Builtin args result)
deriving instance Show (Builtin args result)


builtinName :: Builtin args result -> Name
builtinName = name . pack . ("__semantic_" <>) . headToLower . show
  where headToLower (c:cs) = toLower c : cs
        headToLower ""     = ""


builtin :: ( HasCallStack
           , Members '[ Allocator location value
                      , Reader (Environment location)
                      , Reader ModuleInfo
                      , Reader Span
                      , State (Environment location)
                      , State (Heap location (Cell location) value)
                      ] effects
           , Ord location
           , Reducer value (Cell location value)
           )
        => Builtin args result
        -> Evaluator location value effects value
        -> Evaluator location value effects ()
builtin b def = withCurrentCallStack callStack $ do
  let name = builtinName b
  addr <- alloc name
  modifyEnv (insert name addr)
  def >>= assign addr

lambda :: (AbstractFunction location value effects, Member Fresh effects)
       => (Name -> Evaluator location value effects value)
       -> Evaluator location value effects value
lambda body = do
  var <- nameI <$> fresh
  closure [var] lowerBound (body var)

defineBuiltins :: ( AbstractValue location value effects
                  , HasCallStack
                  , Members '[ Allocator location value
                             , Fresh
                             , Primitive
                             , Reader (Environment location)
                             , Reader ModuleInfo
                             , Reader Span
                             , Resumable (EnvironmentError value)
                             , State (Environment location)
                             , State (Heap location (Cell location) value)
                             ] effects
                  , Ord location
                  , Reducer value (Cell location value)
                  )
               => Evaluator location value effects ()
defineBuiltins =
  builtin Print (lambda (\ v -> variable v >>= asString >>= prim Print . unpack >> unit))


-- | Call a 'Builtin' with parameters.
prim :: (Effectful m, Member Primitive effects) => Builtin args result -> args -> m effects result
prim builtin params = send (Prim builtin params)

data Primitive result where
  Prim :: Builtin args result -> args -> Primitive result

runPrimitive :: (Effectful m, Member Trace effects) => m (Primitive ': effects) a -> m effects a
runPrimitive = interpret (\ (Prim builtin params) -> case builtin of
  Print -> trace params)


data SomeBuiltin where
  SomeBuiltin :: Builtin arg return -> SomeBuiltin

instance Eq SomeBuiltin where
  SomeBuiltin Print == SomeBuiltin Print = True

instance Ord SomeBuiltin where
  SomeBuiltin Print `compare` SomeBuiltin Print = EQ

deriving instance Show SomeBuiltin

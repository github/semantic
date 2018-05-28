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

data Builtin = Print
  deriving (Bounded, Enum, Eq, Ord, Show)

builtinName :: Builtin -> Name
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
        => Builtin
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
                             , Reader (Environment location)
                             , Reader ModuleInfo
                             , Reader Span
                             , Resumable (EnvironmentError location)
                             , State (Environment location)
                             , State (Heap location (Cell location) value)
                             , Trace
                             ] effects
                  , Ord location
                  , Reducer value (Cell location value)
                  )
               => Evaluator location value effects ()
defineBuiltins =
  builtin Print (lambda (\ v -> variable v >>= asString >>= trace . unpack >> pure unit))

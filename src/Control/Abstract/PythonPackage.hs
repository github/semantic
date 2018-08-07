{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances, LambdaCase, MultiWayIf #-}
module Control.Abstract.PythonPackage
( runPythonPackaging, Strategy(..) ) where

import           Control.Abstract.Heap (AddressError, deref, Allocator, Deref)
import           Control.Abstract.Modules (LoadError (..))
import           Control.Abstract.Value
import           Control.Monad.Effect.Internal (raiseHandler, lowerHandler)
import           Control.Monad.Effect (Effectful (..), Effects, interpose)
import           qualified Control.Monad.Effect as Eff
import           Data.Abstract.Evaluatable (EvalError, Evaluator, Resumable, variable, State, runState, put, get, Env, PackageInfo, Fresh, Trace, trace)
import Control.Abstract.Evaluator (LoopControl, Return)
import           Data.Abstract.Module hiding (Module)
import           Data.Abstract.Name (Name (..), name)
import           Data.Abstract.Path (stripQuotes)
import           Data.Abstract.Value.Concrete (Value(..), ValueError(..))
import Data.Abstract.Address
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import           Data.Term
import           Prologue
import Data.Coerce

data Strategy = Unknown | Packages [Text] | FindPackages [Text]
  deriving (Show)

runPythonPackaging :: forall effects address body a. (
                      Eff.PureEffects effects
                      , Show address
                      , Member (Resumable (ValueError address body)) effects
                      , Member Fresh effects
                      , Coercible body (Eff.Eff effects)
                      , Member (State Strategy) effects
                      , Member Trace effects
                      , Member (Allocator address (Value address body)) effects
                      , Member (Deref address (Value address body)) effects
                      , Member (Env address) effects
                      , Member (Eff.Exc (LoopControl address)) effects
                      , Member (Eff.Exc (Return address)) effects
                      , Member (Eff.Reader ModuleInfo) effects
                      , Member (Eff.Reader PackageInfo) effects
                      , Member (Function address (Value address body)) effects)
                   => Evaluator address (Value address body) effects a
                   -> Evaluator address (Value address body) effects a
runPythonPackaging evaluator = (Eff.interpose @(Function address (Value address body)) $ \case
  (Call callName params) -> do
    case callName of
      Closure _ _ name' paramNames _ _ -> do
        let bindings = foldr (\ (name, addr) rest -> Map.insert name addr rest) lowerBound (zip paramNames params)

        case name' of
          Just n | name "find_packages" == n -> do
            case Map.lookup (name "exclude") bindings of
              Just address -> do
                as <- (asArray <=< deref) address
                as' <- traverse (asString <=< deref) as
                put (FindPackages (stripQuotes <$> as'))
              _ -> trace "In PythonPackaging" >> put (FindPackages [])
          Just n | name "setup" == n -> do
            packageState <- get
            case packageState of
              Unknown -> case Map.lookup (name "packages") bindings of
                Just address -> do
                  as <- (asArray <=< deref) address
                  as' <- traverse (asString <=< deref) as
                  put (Packages (stripQuotes <$> as'))
                _ -> pure ()
              _ -> pure ()
          _ -> pure ()
      _ -> pure ()
    call callName params
  (Function name params vars body) ->  function name params vars (raiseEff body)
  ) evaluator

{-# LANGUAGE GADTs, LambdaCase, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.PythonPackage
( runPythonPackaging, Strategy(..) ) where

import           Control.Abstract.Evaluator (LoopControl, Return)
import           Control.Abstract.Heap (Allocator, Deref, deref)
import           Control.Abstract.Value
import           Control.Monad.Effect (Effectful (..))
import qualified Control.Monad.Effect as Eff
import           Data.Abstract.Evaluatable (Env, Evaluator, Fresh, PackageInfo, Resumable, State, get, put)
import           Data.Abstract.Module hiding (Module)
import           Data.Abstract.Name (name)
import           Data.Abstract.Path (stripQuotes)
import           Data.Abstract.Value.Concrete (Value (..), ValueError (..))
import           Data.Coerce
import qualified Data.Map as Map
import           Prologue

data Strategy = Unknown | Packages [Text] | FindPackages [Text]
  deriving (Show, Eq)

runPythonPackaging :: forall effects address body a. (
                      Eff.PureEffects effects
                      , Show address
                      , Member (Resumable (ValueError address body)) effects
                      , Member Fresh effects
                      , Coercible body (Eff.Eff effects)
                      , Member (State Strategy) effects
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
runPythonPackaging = Eff.interpose @(Function address (Value address body)) $ \case
  Call callName super params -> do
    case callName of
      Closure _ _ name' paramNames _ _ -> do
        let bindings = foldr (\ (name, addr) rest -> Map.insert name addr rest) lowerBound (zip paramNames params)
        let asStrings address = (deref >=> asArray) address >>= traverse (deref >=> asString)

        case name' of
          Just n
            | name "find_packages" == n -> do
              as <- maybe (pure mempty) (fmap (fmap stripQuotes) . asStrings) (Map.lookup (name "exclude") bindings)
              put (FindPackages as)
            | name "setup" == n -> do
              packageState <- get
              if packageState == Unknown then do
                as <- maybe (pure mempty) (fmap (fmap stripQuotes) . asStrings) (Map.lookup (name "packages") bindings)
                put (Packages as)
                else
                  pure ()
          _ -> pure ()
      _ -> pure ()
    call callName super params
  Function name params vars body ->  function name params vars (raiseEff body)

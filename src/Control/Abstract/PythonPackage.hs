{-# LANGUAGE GADTs, LambdaCase, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.PythonPackage
( runPythonPackaging, Strategy(..) ) where

import           Control.Abstract.Evaluator (LoopControl, Return)
import Control.Abstract.ScopeGraph (Allocator)
import           Control.Abstract.Heap (Deref)
import           Control.Abstract.Value
import           Control.Monad.Effect (Effectful (..))
import qualified Control.Monad.Effect as Eff
import           Data.Abstract.Evaluatable
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
                      , Ord address
                      , Show address
                      , Member Trace effects
                      , Member (Boolean (Value address body)) effects
                      , Member (State (Heap address address (Value address body))) effects
                      , Member (Resumable (BaseError (AddressError address (Value address body)))) effects
                      , Member (Resumable (BaseError (ValueError address body))) effects
                      , Member Fresh effects
                      , Coercible body (Eff.Eff effects)
                      , Member (State Strategy) effects
                      , Member (Allocator address) effects
                      , Member (Deref (Value address body)) effects
                      , Member (Eff.Exc (LoopControl (Value address body))) effects
                      , Member (Eff.Exc (Return (Value address body))) effects
                      , Member (Eff.Reader ModuleInfo) effects
                      , Member (Eff.Reader PackageInfo) effects
                      , Member (Eff.Reader Span) effects
                      , Member (Function address (Value address body)) effects)
                   => Evaluator address (Value address body) effects a
                   -> Evaluator address (Value address body) effects a
runPythonPackaging = Eff.interpose @(Function address (Value address body)) $ \case
  Call callName super params -> do
    case callName of
      Closure _ _ name' paramNames _ _ -> do
        let bindings = foldr (\ (name, value) rest -> Map.insert name value rest) lowerBound (zip paramNames params)
        let asStrings address = asArray address >>= traverse asString

        if name "find_packages" == name' then do
          as <- maybe (pure mempty) (fmap (fmap stripQuotes) . asStrings) (Map.lookup (name "exclude") bindings)
          put (FindPackages as)
        else if name "setup" == name' then do
          packageState <- get
          if packageState == Unknown then do
            as <- maybe (pure mempty) (fmap (fmap stripQuotes) . asStrings) (Map.lookup (name "packages") bindings)
            put (Packages as)
            else
              pure ()
        else pure ()
      _ -> pure ()
    call callName super params
  Function name params vars body ->  function name params vars (raiseEff body)

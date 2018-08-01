{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances, LambdaCase #-}
module Control.Abstract.PythonPackage
( runPythonPackaging, Strategy(..) ) where

import           Control.Abstract.Heap (AddressError)
import           Control.Abstract.Modules (LoadError (..))
import           Control.Abstract.Value
import           Control.Monad.Effect.Internal (raiseHandler, lowerHandler)
import           Control.Monad.Effect (Effectful (..), Effects, interpose)
import           qualified Control.Monad.Effect as Eff
import           Data.Abstract.Evaluatable (EvalError, Evaluator, Resumable, variable, State, runState, put)
import           Data.Abstract.Module hiding (Module)
import           Data.Abstract.Name (Name (..), name)
import           Data.Abstract.Path (stripQuotes)
import           Data.Abstract.Value.Concrete (Value)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.List as List
import           Data.Term
import           Prologue

data Strategy = Unknown | Packages [Text] | FindPackages [Text]
  deriving (Show)

runPythonPackaging :: forall address value effects a. (
                      Eff.PureEffects effects
                      , Member (Resumable (AddressError address value)) effects
                      , Member (Resumable EvalError) effects
                      , Member (State Strategy) effects
                      , Member (Function address value) effects)
                   => Evaluator address value effects a
                   -> Evaluator address value effects a
runPythonPackaging = (Eff.interpose @(Function address value) $ \case
  (Call callName params) -> do

    name' <- asString callName
    params' <- traverse asPair params

    case name' of
      "find_packages" -> do
        case List.lookup (name "exclude") params' of
          Just value -> do
            as <- array value
            as' <- traverse asString as
            (put (FindPackages (stripQuotes <$> as')))
          Nothing -> pure ()
      "setup" -> do
        case List.lookup (name "packages") params' of
          Just value -> do
            as <- asArray value
            as' <- traverse asString as
            raise (put (Packages (stripQuotes <$> as')))
          Nothing -> pure ()
      _ -> raise (put Unknown)
    call callName params
  (Function params vars body) -> function params vars (raiseEff body)
  )

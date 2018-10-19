{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.PythonPackage
( runPythonPackaging, Strategy(..) ) where

import           Control.Abstract.Evaluator (LoopControl, Return)
import           Control.Abstract.Heap (Allocator, Deref, deref)
import           Control.Abstract.Value
import           Data.Abstract.Evaluatable
import           Data.Abstract.Name (name)
import           Data.Abstract.Path (stripQuotes)
import           Data.Abstract.Value.Concrete (Value (..), ValueError (..))
import qualified Data.Map as Map
import           Prologue

data Strategy = Unknown | Packages [Text] | FindPackages [Text]
  deriving (Show, Eq)

runPythonPackaging :: ( Carrier sig m
                      , Ord address
                      , Show address
                      , Show term
                      , Member Trace sig
                      , Member (Boolean (Value term address)) sig
                      , Member (State (Heap address (Value term address))) sig
                      , Member (Resumable (BaseError (AddressError address (Value term address)))) sig
                      , Member (Resumable (BaseError (ValueError term address))) sig
                      , Member Fresh sig
                      , Member (State Strategy) sig
                      , Member (Allocator address) sig
                      , Member (Deref (Value term address)) sig
                      , Member (Env address) sig
                      , Member (Error (LoopControl address)) sig
                      , Member (Error (Return address)) sig
                      , Member (Reader ModuleInfo) sig
                      , Member (Reader PackageInfo) sig
                      , Member (Reader Span) sig
                      , Member (Function term address (Value term address)) sig
                      )
                   => Evaluator term address (Value term address) (InterposeC (Function term address (Value term address))
                     (Evaluator term address (Value term address) m)) a
                   -> Evaluator term address (Value term address) m a
runPythonPackaging = interpose (\case
  Call callName super params k -> k =<< do
    case callName of
      Closure _ _ name' paramNames _ _ -> do
        let bindings = foldr (\ (name, addr) -> Map.insert name addr) lowerBound (zip paramNames params)
        let asStrings = deref >=> asArray >=> traverse (deref >=> asString)

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
  Function name params body k -> function name params body >>= k
  BuiltIn b k -> builtIn b >>= k)
  . runEvaluator

interpose :: (Member eff sig, HFunctor eff, Carrier sig m)
          => (forall v. eff m (m v) -> m v)
          -> Eff (InterposeC eff m) a
          -> m a
interpose handler = runInterposeC handler . interpret

newtype InterposeC eff m a = InterposeC ((forall x . eff m (m x) -> m x) -> m a)

runInterposeC :: (forall x . eff m (m x) -> m x) -> InterposeC eff m a -> m a
runInterposeC f (InterposeC m) = m f

instance (Member eff sig, HFunctor eff, Carrier sig m) => Carrier sig (InterposeC eff m) where
  gen a = InterposeC (const (gen a))
  alg op
    | Just e <- prj op = InterposeC (\ handler -> handler (handlePure (runInterposeC handler) e))
    | otherwise        = InterposeC (\ handler -> alg (handlePure (runInterposeC handler) op))

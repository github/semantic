{-# LANGUAGE UndecidableInstances #-}
module Control.Abstract.PythonPackage
( runPythonPackaging, Strategy(..) ) where

import           Control.Abstract as Abstract
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Data.Abstract.Evaluatable
import           Data.Abstract.Name (name)
import           Data.Abstract.Path (stripQuotes)
import           Data.Abstract.Value.Concrete (Value (..), ValueError (..))
import qualified Data.Map as Map
import           Prologue

data Strategy = Unknown | Packages [Text] | FindPackages [Text]
  deriving (Show, Eq)

runPythonPackaging :: ( Carrier sig m
                      , Show address
                      , Show term
                      , Member (Boolean (Value term address)) sig
                      , Member (Abstract.String (Value term address)) sig
                      , Member (Resumable (BaseError (ValueError term address))) sig
                      , Member (State Strategy) sig
                      , Member (Reader ModuleInfo) sig
                      , Member (Reader Span) sig
                      , Member (Function term address (Value term address)) sig)
                   => Evaluator term address (Value term address) (PythonPackagingC term address (Eff m)) a
                   -> Evaluator term address (Value term address) m a
runPythonPackaging = raiseHandler (runPythonPackagingC . interpret)


newtype PythonPackagingC term address m a = PythonPackagingC { runPythonPackagingC :: m a }

wrap :: Evaluator term address (Value term address) m a -> PythonPackagingC term address (Eff m) a
wrap = PythonPackagingC . runEvaluator

instance ( Carrier sig m
         , Member (Boolean (Value term address)) sig
         , Member (Function term address (Value term address)) sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (ValueError term address))) sig
         , Member (State Strategy) sig
         , Member (Abstract.String (Value term address)) sig
         , Show address
         , Show term
         )
      => Carrier sig (PythonPackagingC term address (Eff m)) where
  ret = PythonPackagingC . ret
  eff op
    | Just e <- prj op = wrap $ case handleCoercible e of
      Call callName params k -> Evaluator . k =<< do
        case callName of
          Closure _ _ name' _ paramNames _ _ _ -> do
            let bindings = foldr (uncurry Map.insert) lowerBound (zip paramNames params)
            let asStrings = asArray >=> traverse asString

            if Just (name "find_packages") == name' then do
              as <- maybe (pure mempty) (fmap (fmap stripQuotes) . asStrings) (Map.lookup (name "exclude") bindings)
              put (FindPackages as)
            else if Just (name "setup") == name' then do
              packageState <- get
              if packageState == Unknown then do
                as <- maybe (pure mempty) (fmap (fmap stripQuotes) . asStrings) (Map.lookup (name "packages") bindings)
                put (Packages as)
                else
                  pure ()
            else pure ()
          _ -> pure ()
        call callName params
      Function name params body scope k -> function name params body scope >>= Evaluator . k
      BuiltIn n b k -> builtIn n b >>= Evaluator . k
      Bind obj value k -> bindThis obj value >>= Evaluator . k
    | otherwise        = PythonPackagingC (eff (handleCoercible op))

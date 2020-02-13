{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Abstract.PythonPackage
( runPythonPackaging, Strategy(..) ) where

import           Analysis.Name (name)
import           Control.Abstract as Abstract
import           Control.Algebra
import           Control.Effect.Sum.Project
import           Control.Monad
import           Data.Abstract.Path (stripQuotes)
import           Data.Abstract.Value.Concrete (Value (..))
import qualified Data.Map as Map
import           Data.Semilattice.Lower
import           Data.Text (Text)

data Strategy = Unknown | Packages [Text] | FindPackages [Text]
  deriving (Show, Eq)

runPythonPackaging :: Evaluator term address (Value term address) (PythonPackagingC term address m) a
                   -> Evaluator term address (Value term address) m a
runPythonPackaging = raiseHandler runPythonPackagingC


newtype PythonPackagingC term address m a = PythonPackagingC { runPythonPackagingC :: m a }
  deriving (Applicative, Functor, Monad)

wrap :: Evaluator term address (Value term address) m a -> PythonPackagingC term address m a
wrap = PythonPackagingC . runEvaluator

instance ( Algebra sig m
         , Project (Function term address (Value term address)) sig
         , Has (Function term address (Value term address)) sig m
         , Has (State Strategy) sig m
         , Has (Abstract.String (Value term address)) sig m
         , Has (Abstract.Array (Value term address)) sig m
         )
      => Algebra sig (PythonPackagingC term address m) where
  alg op
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
              if packageState == Control.Abstract.PythonPackage.Unknown then do
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
    | otherwise = PythonPackagingC . alg $ handleCoercible op

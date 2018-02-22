{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses #-}
module Analysis.Abstract.Evaluating3 where

import Control.Effect
import Control.Monad.Effect (Eff, Members)
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.Store2
import Control.Monad.Effect.State
import Control.Monad.Effect.Reader
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Linker
import Data.Abstract.FreeVariables
import Data.Abstract.Eval3
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Abstract.Live
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import qualified Data.Map as Map
import Data.Semigroup
import Prelude hiding (fail)
import Data.Blob
import Data.Maybe (fromMaybe)
import System.FilePath.Posix
import Control.Monad.Effect.Embedded

require :: forall v es. Members (Evaluating v) es => FilePath -> Eff es v
require name = do
  linker <- ask @(Linker (Evaluator v))
  maybe (fail ("cannot find " <> show name)) (raiseEmbedded . runEvaluator) (linkerLookup name linker)

-- | The effects necessary for concrete interpretation.
type Evaluating v
  = '[ Fail
     , State (Store (LocationFor v) v)
     , State (EnvironmentFor v)
     , Reader (EnvironmentFor v)
     , Reader (Linker (Evaluator v))
     ]

newtype Evaluator v = Evaluator { runEvaluator :: Eff (Evaluating v) v }

-- | Evaluate a term to a value.
evaluate :: forall v term.
          ( Ord v
          , Ord (LocationFor v) -- For 'MonadStore'
          , Recursive term
          , Evaluatable (Evaluating v) term v (Base term)
          )
         => term
         -> Final (Evaluating v) v
evaluate = run @(Evaluating v) . fix (const step)

-- evaluates :: forall v term
--           . ( Ord v
--             , Ord (LocationFor v) -- For 'MonadStore'
--             , Recursive term
--             , Evaluatable (Evaluating term v) term v (Base term)
--             -- , Ord (Cell (LocationFor v) v)
--             -- , Semigroup (Cell (LocationFor v) v)
--             -- , Functor (Base term)
--             -- , Recursive term
--             -- , AbstractValue v
--             -- , MonadAddress (LocationFor v) (Eff (Evaluating v))
--             -- , FreeVariables term
--             -- , Eval term v (Eff (Evaluating v)) (Base term)
--             )
--           => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
--           -> (Blob, term)   -- Entrypoint
--           -> Final (Evaluating term v) v
-- evaluates pairs = run @(Evaluating term v) . fix go
--   where
--     go :: ((Blob, term) -> Eff (Evaluating term v) (Final (Evaluating term v) v)) -> (Blob, term) -> Eff (Evaluating term v) (Final (Evaluating term v) v)
--     go recur (b@Blob{..}, t) = do
--       local (const (Linker (Map.fromList (map (toPathActionPair recur) pairs)))) (step t)
--
--     toPathActionPair recur p@(Blob{..}, t) = do
--       v <- run @(Evaluating term v) (fix (const (step t)))
--       pure (dropExtensions blobPath, fst v)

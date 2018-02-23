{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses #-}
module Analysis.Abstract.Evaluating where

import Control.Effect
import Control.Monad.Effect (Eff, Members)
import Control.Monad.Effect.Embedded
import Control.Monad.Effect.Evaluatable
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Linker
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Abstract.FreeVariables
import Data.Blob
import Data.Traversable
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Foldable (toList)
import Data.Semigroup
import Prelude hiding (fail)
import qualified Data.Map as Map
import System.FilePath.Posix
import Data.Union

import qualified Data.ByteString.Char8 as BC
import Debug.Trace

-- | The effects necessary for concrete interpretation.
--
-- NOTE: Uses a memoizing linker strategy.
type Evaluating t v
  = '[ Fail
     , State (Store (LocationFor v) v)
     , State (EnvironmentFor v)  -- Global (imperative) environment
     , Reader (EnvironmentFor v) -- Local environment (e.g. binding over a closure)
     , State (Linker t v)
     ]

-- | Require/import another file and return an Effect.
require :: forall v term es.
        ( Member Fail es
        , Member (State (Linker term v)) es
        , Evaluatable es term v (Base term)
        , Recursive term
        , FreeVariables term)
        => term -> Eff es v
require term = do
  let [name'] = toList (freeVariables term)
  let name = BC.unpack name'
  linker <- get @(Linker term v)
  maybe (evalModule linker name) (trace ("require:" <> name) pure) (linkerLookupValue name linker)
  where
    evalModule linker name = case linkerLookupTerm name linker of
      Just m -> do
        v <- step @v m
        modify @(Linker term v) (linkerInsert name v)
        trace ("require[eval]:" <> name) (pure v)
      _ -> fail ("cannot find " <> show name)

-- | Evaluate a term to a value.
evaluate :: forall v term.
         ( Ord v
         , Ord (LocationFor v)
         , Evaluatable (Evaluating term v) term v (Base term)
         , Recursive term
         )
         => term
         -> Final (Evaluating term v) v
evaluate = run @(Evaluating term v) . fix (const step)

-- | Evaluate terms and an entry point to a value.
evaluates :: forall v term.
          ( Ord v
          , Ord (LocationFor v)
          , Evaluatable (Evaluating term v) term v (Base term)
          , Recursive term
          )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (Evaluating term v) v
evaluates pairs = run @(Evaluating term v) . fix go
  where
    go _ (Blob{..}, t) = do
      put (Linker @term @v Map.empty (Map.fromList (fmap toPathActionPair pairs)))
      trace ("step[entryPoint]: " <> show blobPath) (step @v t)
    toPathActionPair (Blob{..}, t) = (dropExtensions blobPath, t)


-- | The effects necessary for concrete interpretation.
--
-- NOTE: Uses a preload linker strategy.
type Evaluating' v
  = '[ Fail
     , State (Store (LocationFor v) v)
     , State (EnvironmentFor v)  -- Global (imperative) environment
     , Reader (EnvironmentFor v) -- Local environment (e.g. binding over a closure)
     , Reader (Linker' v)
     ]

-- | Require/import another file and return an Effect.
require' :: forall v term es.
         ( Member Fail es
         , Member (Reader (Linker' v)) es
         , FreeVariables term
         )
         => term -> Eff es v
require' term = do
  let [name'] = toList (freeVariables term)
  let name = BC.unpack name'
  linker <- ask
  maybe (fail ("cannot find " <> show name)) pure (linkerLookup name linker)

-- | Evaluate a term to a value.
evaluate' :: forall v term.
        ( Ord v
        , Ord (LocationFor v)
        , Evaluatable (Evaluating' v) term v (Base term)
        , Recursive term
        )
        => term
        -> Final (Evaluating' v) v
evaluate' = run @(Evaluating' v) . fix (const step)

-- | Evaluate terms and an entry point to a value.
evaluates' :: forall v term.
         ( Ord v
         , Ord (LocationFor v)
         , Evaluatable (Evaluating' v) term v (Base term)
         , Recursive term
         )
         => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
         -> (Blob, term)   -- Entrypoint
         -> Final (Evaluating' v) v
evaluates' pairs = run @(Evaluating' v) . fix go
 where
   go _ (Blob{..}, t) = do
     modules <- for pairs $ \(Blob{..}, t) -> do
       v <- trace ("step: " <> show blobPath) $ step @v t
       pure (dropExtensions blobPath, v)
     local (const (Linker' (Map.fromList modules))) (trace ("step: " <> show blobPath) (step @v t))


-- | The effects necessary for concrete interpretation.
--
-- NOTE: Uses a lazy, non-memoized linker strategy.
type Evaluating'' v
  = '[ Fail
     , State (Store (LocationFor v) v)
     , State (EnvironmentFor v)  -- Global (imperative) environment
     , Reader (EnvironmentFor v) -- Local environment (e.g. binding over a closure)
     , Reader (Linker' (Evaluator v))
     ]

newtype Evaluator v = Evaluator { runEvaluator :: Eff (Evaluating'' v) v }

-- | Require/import another file and return an Effect.
require'' :: forall v term es.
        ( Members (Evaluating'' v) es
        , FreeVariables term
        )
        => term -> Eff es v
require'' term = do
  let [name'] = toList (freeVariables term)
  let name = BC.unpack name'
  linker <- trace ("require: " <> show name) $ ask @(Linker' (Evaluator v))
  maybe (fail ("cannot find " <> show name)) (raiseEmbedded . runEvaluator) (linkerLookup name linker)

-- | Evaluate a term to a value.
evaluate'' :: forall v term.
         ( Ord v
         , Ord (LocationFor v)
         , Evaluatable (Evaluating'' v) term v (Base term)
         , Recursive term
         )
         => term
         -> Final (Evaluating'' v) v
evaluate'' = run @(Evaluating'' v) . fix (const step)

-- | Evaluate terms and an entry point to a value.
evaluates'' :: forall v term.
          ( Ord v
          , Ord (LocationFor v)
          , Evaluatable (Evaluating'' v) term v (Base term)
          , Recursive term
          )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (Evaluating'' v) v
evaluates'' pairs = run @(Evaluating'' v) . fix go
  where
    go _ (Blob{..}, t) = local (const (Linker' (Map.fromList (map toPathActionPair pairs)))) (trace ("step: " <> show blobPath) (step @v t))
    toPathActionPair (Blob{..}, t) = (dropExtensions blobPath, Evaluator (step @v t))

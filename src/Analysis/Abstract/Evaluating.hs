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
        v <- para eval m
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
evaluate = run @(Evaluating term v) . para eval

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
evaluates pairs (Blob{..}, t) = run @(Evaluating term v) $ do
  put (Linker @term @v Map.empty (Map.fromList (fmap toPathActionPair pairs)))
  trace ("step[entryPoint]: " <> show blobPath) (para eval t)
  where
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
evaluate' = run @(Evaluating' v) . para eval

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
evaluates' pairs (Blob{..}, t) = run @(Evaluating' v) $ do
  modules <- for pairs $ \(Blob{..}, t) -> do
    v <- trace ("step: " <> show blobPath) $ para eval t
    pure (dropExtensions blobPath, v :: v)
  local (const (Linker' (Map.fromList modules))) (trace ("step: " <> show blobPath) (para eval t))

-- | The effects necessary for concrete interpretation.
--
-- NOTE: Uses a lazy, non-memoized linker strategy where Effects are stored in the linker and run each time they are needed.
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
evaluate'' = run @(Evaluating'' v) . para eval

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
evaluates'' pairs (Blob{..}, t) = run @(Evaluating'' v) $ local @(Linker' (Evaluator v)) (const (Linker' (Map.fromList (map toPathActionPair pairs)))) (trace ("step: " <> show blobPath) (para eval t))
  where
    toPathActionPair (Blob{..}, t) = (dropExtensions blobPath, Evaluator (para eval t))


-- | The effects necessary for concrete interpretation.
--
-- NOTE: Allows for both the concepts of requiring and loading.
--   * require - evaluates the specified term and memoizes the resulting value (and environment), future calls to require the same file do not re-evaluate.
--   * load    - always evaluates the specified term.
type Evaluating''' v
  = '[ Fail
     , State (Store (LocationFor v) v)
     , State (EnvironmentFor v)  -- Global (imperative) environment
     , Reader (EnvironmentFor v) -- Local environment (e.g. binding over a closure)
     , State (Module (Evaluator' v) v)
     ]

newtype Evaluator' v = Evaluator' { unEvaluator' :: Eff (Evaluating''' v) v }

-- | Load another file and return an Effect.
load :: forall v term es.
        ( Members (Evaluating''' v) es
        , FreeVariables term
        )
        => term -> Eff es v
load term = do
  let name = moduleName term
  module' <- get @(Module (Evaluator' v) v)
  maybe (fail ("cannot find " <> show name)) (raiseEmbedded . unEvaluator' . Prelude.fst) (moduleLookup name module')

-- | Require/import another file and return an Effect.
require''' :: forall v term es.
        ( Members (Evaluating''' v) es
        , FreeVariables term
        )
        => term -> Eff es v
require''' term = do
  let name = moduleName term
  module' <- get @(Module (Evaluator' v) v)
  case moduleLookup name module' of
    Just (m, Nothing) -> do
      v <- raiseEmbedded (unEvaluator' m)
      modify @(Module (Evaluator' v) v) (moduleInsert name v)
      pure v
    Just (_, Just v) -> pure v
    _ -> fail ("cannot find " <> show name)

moduleName :: FreeVariables term => term -> Prelude.String
moduleName term = let [name'] = toList (freeVariables term) in BC.unpack name'

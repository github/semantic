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

-- -- | The effects necessary for concrete interpretation.
-- --
-- -- NOTE: Uses a memoizing linker strategy.
-- type Evaluating t v
--   = '[ Fail
--      , State (Store (LocationFor v) v)
--      , State (EnvironmentFor v)  -- Global (imperative) environment
--      , Reader (EnvironmentFor v) -- Local environment (e.g. binding over a closure)
--      , State (Linker t v)
--      ]
--
-- -- | Require/import another file and return an Effect.
-- require :: forall v term es.
--         ( Member Fail es
--         , Member (State (Linker term v)) es
--         , Evaluatable es term v (Base term)
--         , Recursive term
--         , FreeVariables term)
--         => term -> Eff es v
-- require term = do
--   let [name'] = toList (freeVariables term)
--   let name = BC.unpack name'
--   linker <- get @(Linker term v)
--   maybe (evalModule linker name) pure (linkerLookupValue name linker)
--   where
--     evalModule linker name = case linkerLookupTerm name linker of
--       Just m -> do
--         v <- step @v m
--         modify @(Linker term v) (linkerInsert name v)
--         pure v
--       _ -> fail ("cannot find " <> show name)
--
-- -- | Evaluate a term to a value.
-- evaluate :: forall v term.
--          ( Ord v
--          , Ord (LocationFor v)
--          , Evaluatable (Evaluating term v) term v (Base term)
--          , Recursive term
--          )
--          => term
--          -> Final (Evaluating term v) v
-- evaluate = run @(Evaluating term v) . fix (const step)
--
-- -- | Evaluate terms and an entry point to a value.
-- evaluates :: forall v term.
--           ( Ord v
--           , Ord (LocationFor v)
--           , Evaluatable (Evaluating term v) term v (Base term)
--           , Recursive term
--           )
--           => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
--           -> (Blob, term)   -- Entrypoint
--           -> Final (Evaluating term v) v
-- evaluates pairs = run @(Evaluating term v) . fix go
--   where
--     go _ (Blob{..}, t) = do
--       put (Linker @term @v Map.empty (Map.fromList (fmap toPathActionPair pairs)))
--       step @v t
--     toPathActionPair (Blob{..}, t) = (dropExtensions blobPath, t)


-- -- | The effects necessary for concrete interpretation.
-- --
-- -- NOTE: Uses a lazy, non-memoized linker strategy where Effects are stored in the linker and run each time they are needed.
-- type Evaluating v
--   = '[ Fail
--      , State (Store (LocationFor v) v)
--      , State (EnvironmentFor v)  -- Global (imperative) environment
--      , Reader (EnvironmentFor v) -- Local environment (e.g. binding over a closure)
--      , Reader (Linker (Evaluator v))
--      ]
--
-- newtype Evaluator v = Evaluator { runEvaluator :: Eff (Evaluating v) v }
--
-- -- | Require/import another file and return an Effect.
-- require :: forall v term es.
--         ( Members (Evaluating v) es
--         , FreeVariables term
--         )
--         => term -> Eff es v
-- require term = do
--   let [name'] = toList (freeVariables term)
--   let name = BC.unpack name'
--   linker <- ask @(Linker (Evaluator v))
--   maybe (fail ("cannot find " <> show name)) (raiseEmbedded . runEvaluator) (linkerLookup name linker)
--
-- -- | Evaluate a term to a value.
-- evaluate :: forall v term.
--          ( Ord v
--          , Ord (LocationFor v)
--          , Evaluatable (Evaluating v) term v (Base term)
--          , Recursive term
--          )
--          => term
--          -> Final (Evaluating v) v
-- evaluate = run @(Evaluating v) . fix (const step)
--
-- -- | Evaluate terms and an entry point to a value.
-- evaluates :: forall v term.
--           ( Ord v
--           , Ord (LocationFor v)
--           , Evaluatable (Evaluating v) term v (Base term)
--           , Recursive term
--           )
--           => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
--           -> (Blob, term)   -- Entrypoint
--           -> Final (Evaluating v) v
-- evaluates pairs = run @(Evaluating v) . fix go
--   where
--     go _ (Blob{..}, t) = local (const (Linker (Map.fromList (map toPathActionPair pairs)))) (step @v t)
--     toPathActionPair (Blob{..}, t) = (dropExtensions blobPath, Evaluator (step @v t))



-- | The effects necessary for concrete interpretation.
type Evaluating v
  = '[ Fail
     , State (Store (LocationFor v) v)
     , State (EnvironmentFor v)  -- Global (imperative) environment
     , Reader (EnvironmentFor v) -- Local environment (e.g. binding over a closure)
     , Reader (Linker (Evaluator v))
     , State (Linker v)
     ]

newtype Evaluator v = Evaluator { runEvaluator :: Eff (Evaluating v) v }

-- | Require/import another term/file and return an Effect.
--
-- Looks up the term's name in the cache of evaluated modules first, returns a value if found, otherwise loads/evaluates the module.
require :: forall v term es.
        ( Members (Evaluating v) es
        , FreeVariables term
        )
        => term -> Eff es v
require term = get @(Linker v) >>= maybe (load term) pure . linkerLookup name
  where name = moduleName term

-- | Load another term/file and return an Effect.
--
-- Always loads/evaluates.
load :: forall v term es.
        ( Members (Evaluating v) es
        , FreeVariables term
        )
        => term -> Eff es v
load term = ask @(Linker (Evaluator v)) >>= maybe (fail ("cannot find " <> show name)) evalAndCache . linkerLookup name
  where name = moduleName term
        evalAndCache e = do
          v <- raiseEmbedded (runEvaluator e)
          modify @(Linker v) (linkerInsert name v)
          pure v

-- | Get a module name from a term (expects single free variables).
moduleName :: FreeVariables term => term -> Prelude.String
moduleName term = let [n] = toList (freeVariables term) in BC.unpack n


-- | Evaluate a term to a value.
evaluate :: forall v term.
         ( Ord v
         , Ord (LocationFor v)
         , Evaluatable (Evaluating v) term v (Base term)
         , Recursive term
         )
         => term
         -> Final (Evaluating v) v
evaluate = run @(Evaluating v) . fix (const step)

-- | Evaluate terms and an entry point to a value.
evaluates :: forall v term.
          ( Ord v
          , Ord (LocationFor v)
          , Evaluatable (Evaluating v) term v (Base term)
          , Recursive term
          )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (Evaluating v) v
evaluates pairs = run @(Evaluating v) . fix go
  where
    go _ (Blob{..}, t) = local (const (Linker (Map.fromList (map toPathActionPair pairs)))) (step @v t)
    toPathActionPair (Blob{..}, t) = (dropExtensions blobPath, Evaluator (step @v t))

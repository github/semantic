{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses #-}
module Analysis.Abstract.Evaluating where

import Prologue
import Control.Effect
import Control.Monad.Effect.Addressable
import Control.Monad.Effect.Evaluatable
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Linker
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Abstract.FreeVariables
import Data.Blob
import Prelude hiding (fail)
import qualified Data.Map as Map
import System.FilePath.Posix

import qualified Data.ByteString.Char8 as BC

-- | The effects necessary for concrete interpretation.
type Evaluating t v
  = '[ Fail
     , State (Store (LocationFor v) v)
     , State (EnvironmentFor v)      -- Global (imperative) environment
     , Reader (EnvironmentFor v)     -- Local environment (e.g. binding over a closure)
     , Reader (Linker t) -- Cache of unevaluated modules
     , State (Linker v)              -- Cache of evaluated modules
     ]

-- | Require/import another term/file and return an Effect.
--
-- Looks up the term's name in the cache of evaluated modules first, returns a value if found, otherwise loads/evaluates the module.
require :: ( AbstractValue v
           , Evaluatable (Base term)
           , FreeVariables term
           , MonadAddressable (LocationFor v) v (Evaluator effects term v)
           , MonadFunctionAbstraction term v (Evaluator effects term v)
           , Recursive term
           , Semigroup (Cell (LocationFor v) v)
           )
        => term
        -> Evaluator effects term v v
require term = getModuleTable >>= maybe (load term) pure . linkerLookup name
  where name = moduleName term

-- | Load another term/file and return an Effect.
--
-- Always loads/evaluates.
load :: ( AbstractValue v
        , Evaluatable (Base term)
        , FreeVariables term
        , MonadAddressable (LocationFor v) v m
        , MonadFunctionAbstraction term v m
        , MonadEvaluator term v m
        , Recursive term
        , Semigroup (Cell (LocationFor v) v)
        )
     => term
     -> m v
load term = askModuleTable >>= maybe notFound evalAndCache . linkerLookup name
  where name = moduleName term
        notFound = fail ("cannot find " <> show name)
        evalAndCache e = do
          v <- foldSubterms eval e
          modifyModuleTable (linkerInsert name v)
          pure v

-- | Get a module name from a term (expects single free variables).
moduleName :: FreeVariables term => term -> Prelude.String
moduleName term = let [n] = toList (freeVariables term) in BC.unpack n


-- | Evaluate a term to a value.
evaluate :: forall v term.
         ( Ord (LocationFor v)
         , AbstractValue v
         , Evaluatable (Base term)
         , FreeVariables term
         , MonadAddressable (LocationFor v) v (Evaluator (Evaluating term v) term v)
         , MonadFunctionAbstraction term v (Evaluator (Evaluating term v) term v)
         , Recursive term
         , Semigroup (Cell (LocationFor v) v)
         )
         => term
         -> Final (Evaluating term v) v
evaluate = run @(Evaluating term v) . runEvaluator . foldSubterms eval

-- | Evaluate terms and an entry point to a value.
evaluates :: forall v term.
          ( Ord (LocationFor v)
          , AbstractValue v
          , Evaluatable (Base term)
          , FreeVariables term
          , MonadAddressable (LocationFor v) v (Evaluator (Evaluating term v) term v)
          , MonadFunctionAbstraction term v (Evaluator (Evaluating term v) term v)
          , Recursive term
          , Semigroup (Cell (LocationFor v) v)
          )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (Evaluating term v) v
evaluates pairs (_, t) = run @(Evaluating term v) (runEvaluator (localModuleTable (const (Linker (Map.fromList (map (first (dropExtensions . blobPath)) pairs)))) (foldSubterms eval t)))

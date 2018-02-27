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
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import Data.Foldable (toList)
import Data.Semigroup
import Prelude hiding (fail)
import qualified Data.Map as Map
import System.FilePath.Posix

import qualified Data.ByteString.Char8 as BC

-- | The effects necessary for concrete interpretation.
type Evaluating v
  = '[ Fail
     , State (Store (LocationFor v) v)
     , State (EnvironmentFor v)      -- Global (imperative) environment
     , Reader (EnvironmentFor v)     -- Local environment (e.g. binding over a closure)
     , Reader (Linker (Evaluator v)) -- Linker effects
     , State (Linker v)              -- Cache of evaluated modules
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
load term = ask @(Linker (Evaluator v)) >>= maybe notFound evalAndCache . linkerLookup name
  where name = moduleName term
        notFound = fail ("cannot find " <> show name)
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

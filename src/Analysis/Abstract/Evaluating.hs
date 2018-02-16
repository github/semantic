{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses #-}
module Analysis.Abstract.Evaluating where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Address
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Linker
import Data.Abstract.FreeVariables
import Data.Abstract.Eval
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Abstract.Live
import Data.Function (fix)
import Data.Functor.Foldable (Base, Recursive(..))
import qualified Data.Map as Map
import Data.Semigroup
import Prelude hiding (fail)
import Data.Blob
import System.FilePath.Posix

class Monad m => MonadLinker v m where
  require :: FilePath -> m v

instance (b ~ Eff (Evaluating v)) => MonadLinker v b where
  require name = do
    linker <- ask
    maybe (fail ("cannot find " <> show name)) runEvaluator (linkerLookup name linker)

-- | The effects necessary for concrete interpretation.
type Evaluating v
  = '[ Fail                                   -- For 'MonadFail'.
     , State  (Store (LocationFor v) v)       -- For 'MonadStore'.
     , Reader (Environment (LocationFor v) v) -- For 'MonadEnv'.
     , Reader (Live (LocationFor v) v)        -- For 'MonadGC'.
     , Reader (Linker (Evaluator v))          -- For 'MonadLinker'
     ]

newtype Evaluator v = Evaluator { runEvaluator :: Eff (Evaluating v) v }


-- | Evaluate a term to a value.
evaluate :: forall v term
         . ( Ord v
           , Ord (Cell (LocationFor v) v)
           , Semigroup (Cell (LocationFor v) v)
           , Functor (Base term)
           , Recursive term
           , MonadAddress (LocationFor v) (Eff (Evaluating v))
           , Eval term v (Eff (Evaluating v)) (Base term)
           )
         => term
         -> Final (Evaluating v) v
evaluate = run @(Evaluating v) . fix go pure
  where go recur yield = eval recur yield . project

evaluates :: forall v term
          . ( Ord v
            , Ord (Cell (LocationFor v) v)
            , Semigroup (Cell (LocationFor v) v)
            , Functor (Base term)
            , Recursive term
            , AbstractValue v
            , MonadAddress (LocationFor v) (Eff (Evaluating v))
            , FreeVariables term
            , Eval term v (Eff (Evaluating v)) (Base term)
            )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (Evaluating v) v
evaluates pairs = run @(Evaluating v) . fix go pure
  where
    go recur yield (b@Blob{..}, t) = local (const (Linker (Map.fromList (map (toPathActionPair recur pure) pairs)))) $
        eval (\ev term -> recur ev (b, term)) yield (project t)
    toPathActionPair recur yield (b@Blob{..}, t) = (dropExtensions blobPath, Evaluator (go recur yield (b, t)))

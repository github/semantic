{-# LANGUAGE ConstraintKinds, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module Analysis.Abstract.Evaluating where

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Env
import Control.Monad.Effect.Address
import Control.Monad.Effect.Linker
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
import Data.Functor.Foldable (Base, Recursive(..), ListF(..))
import Data.Semigroup

import Data.Blob
import System.FilePath.Posix

import Debug.Trace

-- | The effects necessary for concrete interpretation.
type Evaluating v
  = '[ Fail                                   -- For 'MonadFail'.
     , State  (Store (LocationFor v) v)       -- For 'MonadStore'.
     , Reader (Environment (LocationFor v) v) -- For 'MonadEnv'.
     , Reader (Live (LocationFor v) v)        -- For 'MonadGC'.
     , Reader (Linker v)                      -- For 'MonadLinker'.
     ]

-- | Evaluate a term to a value.
evaluate :: forall v term
         . ( Ord v
           , Ord (Cell (LocationFor v) v)
           , Semigroup (Cell (LocationFor v) v)
           , Functor (Base term)
           , Recursive term
           , MonadAddress (LocationFor v) (Eff (Evaluating v))
           -- , MonadLinker v (Eff (Evaluating v))
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
            -- , MonadLinker v (Eff (Evaluating v))
            -- , MonadEnv v (Eff (Evaluating v))
            , FreeVariables term
            , Eval term v (Eff (Evaluating v)) (Base term)
            )
          => [(Blob, term)]
          -> Final (Evaluating v) v
evaluates = run @(Evaluating v) . fix go pure
  where
    go _ yield [] = yield unit
    go recur yield [x] = go1 recur yield x
    go recur yield (x@(Blob{..}, _):xs) = do
      v <- go1 recur yield x
      localLinker (linkerInsert (dropExtensions blobPath) v) (go recur yield xs)
    go1 recur yield (b@Blob{..}, t) = trace (show blobPath) $
      eval (\ev term -> recur ev [(b, term)]) yield (project t)

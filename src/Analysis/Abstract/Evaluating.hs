{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, MultiParamTypeClasses, UndecidableInstances #-}
module Analysis.Abstract.Evaluating where

import Control.Abstract.Evaluator
import Control.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Blob
import Data.List (intercalate)
import Data.List.Split (splitWhen)
import Prologue
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import System.FilePath.Posix


-- | The effects necessary for concrete interpretation.
type EvaluationEffects t v
  = '[ Fail                              -- Failure with an error message
     , State (Store (LocationFor v) v)   -- The heap
     , State (EnvironmentFor v)          -- Global (imperative) environment
     , State (Map Name (Name, Maybe (Address (LocationFor v) v)))                -- Set of exports
     , Reader (EnvironmentFor v)         -- Local environment (e.g. binding over a closure)
     , Reader (Linker t)                 -- Cache of unevaluated modules
     , State (Linker (EnvironmentFor v)) -- Cache of evaluated modules
     ]

-- | Evaluate a term to a value.
evaluate :: forall v term
         .  ( Evaluatable (Base term)
            , FreeVariables term
            , MonadAddressable (LocationFor v) v (Evaluation term v)
            , MonadValue term v (Evaluation term v)
            , Ord (LocationFor v)
            , Recursive term
            , Semigroup (Cell (LocationFor v) v)
            )
         => term
         -> Final (EvaluationEffects term v) v
evaluate = run @(EvaluationEffects term v) . runEvaluator . runEvaluation . evaluateTerm

-- | Evaluate terms and an entry point to a value.
evaluates :: forall v term
          .  ( Evaluatable (Base term)
             , FreeVariables term
             , MonadAddressable (LocationFor v) v (Evaluation term v)
             , MonadValue term v (Evaluation term v)
             , Ord (LocationFor v)
             , Recursive term
             , Semigroup (Cell (LocationFor v) v)
             )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final (EvaluationEffects term v) v
evaluates pairs (b, t) = run @(EvaluationEffects term v) (runEvaluator (runEvaluation (withModules b pairs (evaluateTerm t))))

-- | Run an action with the passed ('Blob', @term@) pairs available for imports.
withModules :: (MonadAnalysis term value m, MonadEvaluator term value m) => Blob -> [(Blob, term)] -> m a -> m a
withModules Blob{..} pairs = localModuleTable (const moduleTable)
  where
    moduleTable = Linker (Map.fromList (map (first moduleName) pairs))
    rootDir = dropFileName blobPath
    replacePathSeps str = intercalate "." (splitWhen (== pathSeparator) str)
    moduleName Blob{..} = BC.pack $ replacePathSeps (dropExtensions (makeRelative rootDir blobPath))

-- | An analysis performing concrete evaluation of @term@s to @value@s.
newtype Evaluation term value a = Evaluation { runEvaluation :: Evaluator (EvaluationEffects term value) term value a }
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator term value (Evaluation term value)

instance ( Evaluatable (Base t)
         , FreeVariables t
         , MonadAddressable (LocationFor v) v (Evaluation t v)
         , MonadValue t v (Evaluation t v)
         , Recursive t
         , Semigroup (Cell (LocationFor v) v)
         )
         => MonadAnalysis t v (Evaluation t v) where
  evaluateTerm = foldSubterms eval

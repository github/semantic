{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Evaluating
( type Evaluating
, evaluate
, evaluates
) where

import Control.Abstract.Evaluator
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.NonDet
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Configuration
import Data.Abstract.Evaluatable
import Data.Abstract.Address
import Data.Abstract.ModuleTable
import Data.Abstract.Value
import Data.Blob
import qualified Data.IntMap as IntMap
import Data.Language
import Data.List.Split (splitWhen)
import Prelude hiding (fail)
import Prologue
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import System.FilePath.Posix

-- | Evaluate a term to a value.
evaluate :: forall value term effects
         .  ( effects ~ RequiredEffects term value (Evaluating term value effects)
            , Evaluatable (Base term)
            , FreeVariables term
            , MonadAddressable (LocationFor value) value (Evaluating term value effects)
            , MonadValue value (Evaluating term value effects)
            , Recursive term
            )
         => term
         -> Final effects value
evaluate = runAnalysis @(Evaluating term value) . evaluateModule

-- | Evaluate terms and an entry point to a value.
evaluates :: forall value term effects
          .  ( effects ~ RequiredEffects term value (Evaluating term value effects)
             , Evaluatable (Base term)
             , FreeVariables term
             , MonadAddressable (LocationFor value) value (Evaluating term value effects)
             , MonadValue value (Evaluating term value effects)
             , Recursive term
             )
          => [(Blob, term)] -- List of (blob, term) pairs that make up the program to be evaluated
          -> (Blob, term)   -- Entrypoint
          -> Final effects value
evaluates pairs (b, t) = runAnalysis @(Evaluating term value) (withModules b pairs (evaluateModule t))

-- | Run an action with the passed ('Blob', @term@) pairs available for imports.
withModules :: MonadAnalysis term value m => Blob -> [(Blob, term)] -> m a -> m a
withModules Blob{..} pairs = localModuleTable (const moduleTable)
  where
    moduleTable = ModuleTable (Map.fromListWith (<>) (map (bimap moduleName pure) pairs))
    rootDir = dropFileName blobPath
    moduleName Blob{..} = let path = dropExtensions (makeRelative rootDir blobPath)
     in case blobLanguage of
      -- TODO: Need a better way to handle module registration and resolution
      Just Go -> toName (takeDirectory path) -- Go allows defining modules across multiple files in the same directory.
      _ ->  toName path
    toName str = qualifiedName (fmap BC.pack (splitWhen (== pathSeparator) str))

-- | An analysis evaluating @term@s to @value@s with a list of @effects@ using 'Evaluatable', and producing incremental results of type @a@.
newtype Evaluating term value effects a = Evaluating (Eff effects a)
  deriving (Applicative, Functor, Effectful, Monad)

deriving instance Member Fail      effects => MonadFail   (Evaluating term value effects)
deriving instance Member Fresh     effects => MonadFresh  (Evaluating term value effects)
deriving instance Member NonDetEff effects => Alternative (Evaluating term value effects)
deriving instance Member NonDetEff effects => MonadNonDet (Evaluating term value effects)

-- | Effects necessary for evaluating (whether concrete or abstract).
type EvaluatingEffects term value
  = '[ Fail                                        -- Failure with an error message
     , Reader (EnvironmentFor value)               -- Local environment (e.g. binding over a closure)
     , State  (EnvironmentFor value)               -- Global (imperative) environment
     , State  (HeapFor value)                     -- The heap
     , Reader (ModuleTable [term])                 -- Cache of unevaluated modules
     , State  (ModuleTable (EnvironmentFor value)) -- Cache of evaluated modules

     , State (Map Name (Name, Maybe (Address (LocationFor value) value))) -- Set of exports
     , State (IntMap.IntMap term) -- For jumps
     ]

instance Members '[Fail, State (IntMap.IntMap term)] effects => MonadControl term (Evaluating term value effects) where
  label term = do
    m <- raise get
    let i = IntMap.size m
    raise (put (IntMap.insert i term m))
    pure i

  goto label = IntMap.lookup label <$> raise get >>= maybe (fail ("unknown label: " <> show label)) pure

instance Members '[State (Map Name (Name, Maybe (Address (LocationFor value) value))), Reader (EnvironmentFor value), State (EnvironmentFor value)] effects => MonadEnvironment value (Evaluating term value effects) where
  getGlobalEnv = raise get
  putGlobalEnv = raise . put
  withGlobalEnv s = raise . localState s . lower

  addExport key = raise . modify . Map.insert key
  getExports = raise get
  withExports s = raise . localState s . lower

  askLocalEnv = raise ask
  localEnv f a = raise (local f (lower a))

instance Member (State (HeapFor value)) effects => MonadHeap value (Evaluating term value effects) where
  getHeap = raise get
  putHeap = raise . put

instance Members '[Reader (ModuleTable [term]), State (ModuleTable (EnvironmentFor value))] effects => MonadModuleTable term value (Evaluating term value effects) where
  getModuleTable = raise get
  putModuleTable = raise . put

  askModuleTable = raise ask
  localModuleTable f a = raise (local f (lower a))

instance Members (EvaluatingEffects term value) effects => MonadEvaluator term value (Evaluating term value effects) where
  getConfiguration term = Configuration term mempty <$> askLocalEnv <*> getHeap

instance ( Evaluatable (Base term)
         , FreeVariables term
         , Members (EvaluatingEffects term value) effects
         , MonadAddressable (LocationFor value) value (Evaluating term value effects)
         , MonadValue value (Evaluating term value effects)
         , Recursive term
         )
         => MonadAnalysis term value (Evaluating term value effects) where
  type RequiredEffects term value (Evaluating term value effects) = EvaluatingEffects term value

  analyzeTerm = eval

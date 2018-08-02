{-# LANGUAGE GADTs, KindSignatures, LambdaCase, TypeOperators #-}
module Semantic.REPL
( rubyREPL
) where

import Control.Abstract
import Control.Monad.IO.Class
import Data.Abstract.Address
import Data.Abstract.Evaluatable
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Package
import Data.Abstract.Value.Concrete as Concrete
import Data.Coerce
import Data.Graph (topologicalSort)
import Data.Language as Language
import Data.List (uncons)
import Data.Project
import Parsing.Parser
import Prologue
import Semantic.Graph
import Semantic.IO as IO
import Semantic.Task
import Semantic.Util
import System.FilePath

{-

- stop at every term
  - print an excerpt when we stop
    - need to have the Blobs for this
  - showing variable bindings

- interaction
  - inspecting variables
  - inspecting the heap
  - calling functions?

- control
  - step
  - step within a file
  - other variants?

- breakpoints
  - break on a line
  - break on a span
  - break on a function name

what's the ghci workflow look like?

λ rubyREPL ["/Users/rob/Desktop/test.rb"]
…

-}

data REPL (m :: * -> *) result where
  Prompt :: REPL m String
  Output :: String -> REPL m ()

prompt :: (Effectful m, Member REPL effects) => m effects String
prompt = send Prompt

output :: (Effectful m, Member REPL effects) => String -> m effects ()
output s = send (Output s)


instance PureEffect REPL
instance Effect REPL where
  handleState state handler (Request Prompt k) = Request Prompt (handler . (<$ state) . k)
  handleState state handler (Request (Output s) k) = Request (Output s) (handler . (<$ state) . k)


runREPL :: (Effectful m, MonadIO (m effects), PureEffects effects) => m (REPL ': effects) a -> m effects a
runREPL = interpret $ \case
  Prompt -> liftIO $ do
    putStr "repl: "
    getLine
  Output s -> liftIO (putStrLn s)

rubyREPL = repl (Proxy :: Proxy 'Language.Ruby) rubyParser Language.Ruby

repl proxy parser lang paths = runTaskWithOptions debugOptions $ do
  blobs <- catMaybes <$> traverse IO.readFile (flip File lang <$> paths)
  package <- fmap quieterm <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs lang [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  runEvaluator
    . runTermEvaluator @_ @_ @(Value Precise (REPLEff Precise _))
    . runState lowerBound
    . runFresh 0
    . fmap reassociate
    . runLoadError
    . runUnspecialized
    . runEnvironmentError
    . runEvalError
    . runResolutionError
    . runAddressError
    . runValueError
    . runREPL
    . runReader (packageInfo package)
    . runReader (lowerBound @Span)
    . runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Precise)))))
    . raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
    $ evaluate proxy id (withTermSpans . step) (Concrete.runFunction coerce coerce) modules

step :: Member REPL effects
     => SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
     -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
step recur term = do
  str <- prompt
  output str
  res <- recur term
  output "leaving term"
  pure res


newtype REPLEff address rest a = REPLEff
  { runREPLEff :: Eff (  Function address (Value address (REPLEff address rest))
                      ': Exc (LoopControl address)
                      ': Exc (Return address)
                      ': Env address
                      ': Deref address (Value address (REPLEff address rest))
                      ': Allocator address (Value address (REPLEff address rest))
                      ': Reader ModuleInfo

                      ': Modules address
                      ': Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))
                      ': Reader Span
                      ': Reader PackageInfo
                      ': REPL
                      ': Resumable (ValueError address (REPLEff address rest))
                      ': Resumable (AddressError address (Value address (REPLEff address rest)))
                      ': Resumable ResolutionError
                      ': Resumable EvalError
                      ': Resumable (EnvironmentError address)
                      ': Resumable (Unspecialized (Value address (REPLEff address rest)))
                      ': Resumable (LoadError address)
                      ': Fresh
                      ': State (Heap address Latest (Value address (REPLEff address rest)))
                      ': rest
                       ) a
  }

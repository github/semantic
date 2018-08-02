{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, TypeOperators #-}
module Semantic.REPL
( rubyREPL
) where

import Control.Abstract hiding (List, string)
import Control.Monad.IO.Class
import Data.Abstract.Address
import Data.Abstract.Evaluatable hiding (string)
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Abstract.Package
import Data.Abstract.Value.Concrete as Concrete
import Data.Blob (Blob(..))
import Data.Coerce
import Data.Error (showExcerpt)
import Data.Graph (topologicalSort)
import Data.Language as Language
import Data.List (uncons)
import Data.Project
import Parsing.Parser (rubyParser)
import Prologue
import Semantic.Graph
import Semantic.IO as IO
import Semantic.Task hiding (Error)
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

rubyREPL = repl (Proxy :: Proxy 'Language.Ruby) rubyParser

repl proxy parser paths = runTaskWithOptions debugOptions $ do
  blobs <- catMaybes <$> traverse IO.readFile (flip File (Language.reflect proxy) <$> paths)
  package <- fmap (fmap quieterm) <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy (snd <$> package)
  runEvaluator
    . runREPL
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
    . runReader (packageInfo package)
    . runReader (lowerBound @Span)
    . runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Precise)))))
    . raiseHandler (runModules (ModuleTable.modulePaths (packageModules (snd <$> package))))
    $ evaluate proxy id (withTermSpans . step (fmap (\ (x:|_) -> moduleBody x) <$> ModuleTable.toPairs (packageModules (fst <$> package)))) (Concrete.runFunction coerce coerce) modules

step :: ( Member REPL effects
        , Member (Reader ModuleInfo) effects
        , Member (Reader Span) effects
        )
     => [(ModulePath, Blob)]
     -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
     -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
step blobs recur term = do
  list
  res <- runCommands (recur term)
  output "leaving term"
  pure res
  where list = do
          path <- asks modulePath
          span <- ask
          maybe (pure ()) (\ blob -> output (showExcerpt True span blob "")) (Prelude.lookup path blobs)
        runCommand run Step = run
        runCommand run List = list >> runCommands run
        runCommand run (Error err) = output err >> runCommands run
        runCommands run = do
          str <- prompt
          runCommand run (parseCommand str)


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
                      ': Resumable (ValueError address (REPLEff address rest))
                      ': Resumable (AddressError address (Value address (REPLEff address rest)))
                      ': Resumable ResolutionError
                      ': Resumable EvalError
                      ': Resumable (EnvironmentError address)
                      ': Resumable (Unspecialized (Value address (REPLEff address rest)))
                      ': Resumable (LoadError address)
                      ': Fresh
                      ': State (Heap address Latest (Value address (REPLEff address rest)))
                      ': REPL
                      ': rest
                       ) a
  }


data Command
  = Step
  | List
  | Error String

parseCommand :: String -> Command
parseCommand ":step" = Step
parseCommand ":list" = List
parseCommand other = Error $ "unknown command '" <> other <> "'"

{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.REPL
( rubyREPL
) where

import Control.Abstract hiding (List, string)
import Control.Monad.IO.Class
import Data.Abstract.Address
import Data.Abstract.Environment as Env
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
import Data.List (intercalate, uncons, words)
import Data.Project
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import qualified Data.Time.LocalTime as LocalTime
import Parsing.Parser (rubyParser)
import Prologue hiding (throwError)
import Semantic.Config (logOptionsFromConfig)
import Semantic.Distribute
import Semantic.Graph
import Semantic.IO as IO
import Semantic.Resolution
import Semantic.Task hiding (Error)
import Semantic.Telemetry
import Semantic.Telemetry.Log (LogOptions, Message(..), writeLogMessage)
import Semantic.Util
import System.Console.Haskeline
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
  - abandoning the computation

- control
  - step
  - step within a file
  - continuing until the next breakpoint
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
  Prompt :: REPL m (Maybe String)
  Output :: String -> REPL m ()

prompt :: (Effectful m, Member REPL effects) => m effects (Maybe String)
prompt = send Prompt

output :: (Effectful m, Member REPL effects) => String -> m effects ()
output s = send (Output s)


data Quit = Quit
  deriving Show

instance Exception Quit


instance PureEffect REPL
instance Effect REPL where
  handleState state handler (Request Prompt k) = Request Prompt (handler . (<$ state) . k)
  handleState state handler (Request (Output s) k) = Request (Output s) (handler . (<$ state) . k)


runREPL :: (Effectful m, MonadIO (m effects), PureEffects effects) => m (REPL ': effects) a -> m effects a
runREPL = interpret $ \case
  Prompt   -> liftIO (runInputT settings (getInputLine (cyan <> "repl: " <> plain)))
  Output s -> liftIO (runInputT settings (outputStrLn s))

rubyREPL = repl (Proxy @'Language.Ruby) rubyParser

repl proxy parser paths = defaultConfig debugOptions >>= \ config -> runM . runDistribute . runError @_ @_ @SomeException . runTelemetryIgnoringStat (logOptionsFromConfig config) . runTraceInTelemetry . runReader config . IO.runFiles . runResolution . runTaskF $ do
  blobs <- catMaybes <$> traverse IO.readFile (flip File (Language.reflect proxy) <$> paths)
  package <- fmap (fmap quieterm) <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy (snd <$> package)
  runEvaluator
    . runREPL
    . runReader Always
    . runTermEvaluator @_ @_ @(Value Precise (UtilEff Precise _))
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

runTelemetryIgnoringStat :: (Effectful m, MonadIO (m effects), PureEffects effects) => LogOptions -> m (Telemetry : effects) a -> m effects a
runTelemetryIgnoringStat logOptions = interpret $ \case
  WriteStat{} -> pure ()
  WriteLog level message pairs -> do
    time <- liftIO Time.getCurrentTime
    zonedTime <- liftIO (LocalTime.utcToLocalZonedTime time)
    writeLogMessage logOptions (Message level message pairs zonedTime)

step :: ( Member (Env address) effects
        , Member (Exc SomeException) effects
        , Member REPL effects
        , Member (Reader Break) effects
        , Member (Reader ModuleInfo) effects
        , Member (Reader Span) effects
        , Show address
        )
     => [(ModulePath, Blob)]
     -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
     -> SubtermAlgebra (Base term) term (TermEvaluator term address value effects a)
step blobs recur term = do
  break <- shouldBreak
  if break then do
    list
    runCommands (recur term)
  else
    recur term
  where list = do
          path <- asks modulePath
          span <- ask
          maybe (pure ()) (\ blob -> output (showExcerpt True span blob "")) (Prelude.lookup path blobs)
        help = do
          output "Commands available from the prompt:"
          output ""
          output "  :help, :?                   display this list of commands"
          output "  :list                       show the source code around current breakpoint"
          output "  :step                       single-step after stopping at a breakpoint"
          output "  :continue                   continue evaluation until the next breakpoint"
          output "  :show bindings              show the current bindings"
          output "  :quit, :q, :abandon         abandon the current evaluation and exit the repl"
        showBindings = do
          bindings <- Env.head <$> TermEvaluator getEnv
          output $ intercalate "\n" (uncurry showBinding <$> Env.pairs bindings)
        showBinding name addr = show name <> " = " <> show addr
        runCommand run [":step"] = run
        runCommand run [":continue"] = local (const Never) run
        runCommand run [":list"] = list >> runCommands run
        runCommand run [":show", "bindings"] = showBindings >> runCommands run
        runCommand _   [quit] | quit `elem` [":quit", ":q", ":abandon"] = throwError (SomeException Quit)
        runCommand run [":help"] = help >> runCommands run
        runCommand run [":?"] = help >> runCommands run
        runCommand run [] = runCommands run
        runCommand run other = output ("unknown command '" <> intercalate " " other <> "'") >> output "use :? for help" >> runCommands run
        runCommands run = do
          str <- prompt
          maybe (runCommands run) (runCommand run . words) str


data Break
  = Always
  | Never
  deriving Show

shouldBreak :: Member (Reader Break) effects => TermEvaluator term address value effects Bool
shouldBreak = do
  break <- ask
  pure $ case break of
    Always -> True
    Never  -> False


settings :: Settings IO
settings = Settings
  { complete = noCompletion
  , historyFile = Just "~/.local/semantic/repl_history"
  , autoAddHistory = True
  }

cyan :: String
cyan = "\ESC[1;36m\STX"

plain :: String
plain = "\ESC[0m\STX"

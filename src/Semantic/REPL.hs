{-# LANGUAGE GADTs, KindSignatures, LambdaCase, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.REPL
( rubyREPL
) where

import Control.Abstract hiding (Continue, List, string)
import Control.Monad.IO.Class
import Data.Abstract.Address.Precise as Precise
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
import Data.List (uncons)
import Data.Project
import Data.Quieterm
import Data.Span
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import qualified Data.Time.LocalTime as LocalTime
import Numeric (readDec)
import Parsing.Parser (rubyParser)
import Prologue hiding (throwError)
import Semantic.Config (logOptionsFromConfig)
import Semantic.Distribute
import Semantic.Graph
import Semantic.IO as IO
import Semantic.Resolution
import Semantic.Task hiding (Error)
import Semantic.Telemetry
import Semantic.Timeout
import Semantic.Telemetry.Log (LogOptions, Message(..), writeLogMessage)
import Semantic.Util
import System.Console.Haskeline
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath

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


runREPL :: (Effectful m, MonadIO (m effects), PureEffects effects) => Prefs -> Settings IO -> m (REPL ': effects) a -> m effects a
runREPL prefs settings = interpret $ \case
  Prompt   -> liftIO (runInputTWithPrefs prefs settings (getInputLine (cyan <> "repl: " <> plain)))
  Output s -> liftIO (runInputTWithPrefs prefs settings (outputStrLn s))

rubyREPL = repl (Proxy @'Language.Ruby) rubyParser

repl proxy parser paths = defaultConfig debugOptions >>= \ config -> runM . runDistribute . runTimeout (runM . runDistribute) . runError @_ @_ @SomeException . runTelemetryIgnoringStat (logOptionsFromConfig config) . runTraceInTelemetry . runReader config . IO.runFiles . runResolution . runTaskF $ do
  blobs <- catMaybes <$> traverse IO.readFile (flip File (Language.reflect proxy) <$> paths)
  package <- fmap (fmap quieterm) <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy (snd <$> package)
  homeDir <- liftIO getHomeDirectory
  prefs <- liftIO (readPrefs (homeDir <> "/.haskeline"))
  let settingsDir = homeDir <> "/.local/semantic"
  liftIO $ createDirectoryIfMissing True settingsDir
  let settings = Settings
        { complete = noCompletion
        , historyFile = Just (settingsDir <> "/repl_history")
        , autoAddHistory = True
        }
  runEvaluator
    . runREPL prefs settings
    . fmap snd
    . runState ([] @Breakpoint)
    . runReader Step
    . id @(Evaluator _ Precise (Value Precise (ConcreteEff Precise _)) _ _)
    . runPrintingTrace
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
    . runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Precise)))))
    . runModules (ModuleTable.modulePaths (packageModules (snd <$> package)))
    . runReader (packageInfo package)
    . runState (lowerBound @Span)
    . runReader (lowerBound @Span)
    $ evaluate proxy id (withTermSpans . step (fmap (\ (x:|_) -> moduleBody x) <$> ModuleTable.toPairs (packageModules (fst <$> package)))) (Precise.runAllocator . Precise.runDeref) (Concrete.runBoolean . Concrete.runFunction coerce coerce) modules

-- TODO: REPL for typechecking/abstract semantics
-- TODO: drive the flow from within the REPL instead of from without

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
        , Member (Reader ModuleInfo) effects
        , Member (Reader Span) effects
        , Member (Reader Step) effects
        , Member (State [Breakpoint]) effects
        , Show address
        )
     => [(ModulePath, Blob)]
     -> Open (Open (term -> Evaluator term address value effects a))
step blobs recur0 recur term = do
  break <- shouldBreak
  if break then do
    list
    runCommands (recur0 recur term)
  else
    recur0 recur term
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
          bindings <- Env.head <$> getEnv
          output $ unlines (uncurry showBinding <$> Env.pairs bindings)
        showBinding name addr = show name <> " = " <> show addr
        runCommand run [":step"]     = local (const Step) run
        runCommand run [":continue"] = local (const Continue) run
        runCommand run [":break", s]
          | [(i, "")] <- readDec s = modify' (OnLine i :) >> runCommands run
        -- TODO: :show breakpoints
        -- TODO: :delete breakpoints
        runCommand run [":list"] = list >> runCommands run
        runCommand run [":show", "bindings"] = showBindings >> runCommands run
        -- TODO: show the value(s) in the heap
        -- TODO: can we call functions somehow? Maybe parse expressions with the current parser?
        runCommand _   [quit] | quit `elem` [":quit", ":q", ":abandon"] = throwError (SomeException Quit)
        runCommand run [":help"] = help >> runCommands run
        runCommand run [":?"] = help >> runCommands run
        runCommand run [] = runCommands run
        runCommand run other = output ("unknown command '" <> unwords other <> "'") >> output "use :? for help" >> runCommands run
        runCommands run = do
          str <- prompt
          maybe (runCommands run) (runCommand run . words) str


newtype Breakpoint
  = OnLine Int
  deriving Show

-- FIXME: OnLine should take a module, defaulting to the current module
-- TODO: OnPos, taking a column number as well as line number and module
-- TODO: OnSymbol, taking a function/method name? This could be tricky to implement cross-language

data Step
  = Step
  | Continue
  deriving Show

-- TODO: StepLocal/StepModule

shouldBreak :: (Member (State [Breakpoint]) effects, Member (Reader Span) effects, Member (Reader Step) effects) => Evaluator term address value effects Bool
shouldBreak = do
  step <- ask
  case step of
    Step -> pure True
    Continue -> do
      breakpoints <- get
      span <- ask
      pure (any @[] (matching span) breakpoints)
  where matching Span{..} (OnLine n)
          | n >= posLine spanStart
          , n <= posLine spanEnd   = True
          | otherwise              = False


cyan :: String
cyan = "\ESC[1;36m\STX"

plain :: String
plain = "\ESC[0m\STX"

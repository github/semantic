{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Readline.Haskeline
( -- * Readline effect
  module Control.Effect.Readline
  -- * Readline carrier
, runReadline
, runReadlineWithHistory
, ReadlineC (..)
  -- * Re-exports
, Carrier
, run
, runM
) where

import Control.Effect.Carrier
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.Readline hiding (Carrier)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Coerce
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (renderIO)
import System.Console.Haskeline hiding (Handler, handle)
import System.Console.Terminal.Size as Size
import System.Path ((</>))
import qualified System.Path as Path
import System.Path.Directory
import System.IO (stdout)

runReadline :: MonadException m => Prefs -> Settings m -> ReadlineC m a -> m a
runReadline prefs settings = runInputTWithPrefs prefs (coerce settings) . runM . runReader (Line 0) . runReadlineC

runReadlineWithHistory :: MonadException m => ReadlineC m a -> m a
runReadlineWithHistory block = do
  homeDir <- liftIO getHomeDirectory
  prefs <- liftIO $ readPrefs (Path.toString (homeDir </> Path.relFile ".haskeline"))
  let settingsDir = homeDir </> Path.relDir ".local" </> Path.relDir "semantic-core"
      settings = Settings
        { complete = noCompletion
        , historyFile = Just (Path.toString (settingsDir </> Path.relFile "repl_history"))
        , autoAddHistory = True
        }
  liftIO $ createDirectoryIfMissing True settingsDir

  runReadline prefs settings block

newtype ReadlineC m a = ReadlineC { runReadlineC :: ReaderC Line (LiftC (InputT m)) a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO)

instance MonadException m => Carrier Readline (ReadlineC m) where
  eff (Prompt prompt k) = ReadlineC $ do
    str <- sendM (getInputLine @m (cyan <> prompt <> plain))
    Line line <- ask
    local increment (runReadlineC (k line str))
    where cyan = "\ESC[1;36m\STX"
          plain = "\ESC[0m\STX"
  eff (Print doc k) = do
    s <- maybe 80 Size.width <$> liftIO size
    liftIO (renderIO stdout (layoutSmart defaultLayoutOptions { layoutPageWidth = AvailablePerLine s 0.8 } (doc <> line)))
    k


newtype Line = Line Int

increment :: Line -> Line
increment (Line n) = Line (n + 1)

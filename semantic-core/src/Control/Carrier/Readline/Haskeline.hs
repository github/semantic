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
import Control.Monad.IO.Unlift
import Data.Coerce
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (renderIO)
import System.Console.Haskeline hiding (Handler, handle)
import System.Console.Terminal.Size as Size
import System.Directory
import System.FilePath
import System.IO (stdout)

runReadline :: MonadUnliftIO m => Prefs -> Settings m -> ReadlineC m a -> m a
runReadline prefs settings = runUnliftIOToMonadException . runInputTWithPrefs prefs (coerce settings) . runM . runReader (Line 0) . runReadlineC

runReadlineWithHistory :: MonadUnliftIO m => ReadlineC m a -> m a
runReadlineWithHistory block = do
  homeDir <- liftIO getHomeDirectory
  prefs <- liftIO $ readPrefs (homeDir </> ".haskeline")
  let settingsDir = homeDir </> ".local/semantic-core"
      settings = Settings
        { complete = noCompletion
        , historyFile = Just (settingsDir <> "/repl_history")
        , autoAddHistory = True
        }
  liftIO $ createDirectoryIfMissing True settingsDir

  runReadline prefs settings block

newtype ReadlineC m a = ReadlineC { runReadlineC :: ReaderC Line (LiftC (InputT (UnliftIOToMonadException m))) a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO)

instance MonadUnliftIO m => Carrier (Readline :+: Lift (InputT (UnliftIOToMonadException m))) (ReadlineC m) where
  eff (L (Prompt prompt k)) = ReadlineC $ do
    str <- sendM (getInputLine @(UnliftIOToMonadException m) (cyan <> prompt <> plain))
    Line line <- ask
    local increment (runReadlineC (k line str))
    where cyan = "\ESC[1;36m\STX"
          plain = "\ESC[0m\STX"
  eff (L (Print doc k)) = do
    s <- maybe 80 Size.width <$> liftIO size
    liftIO (renderIO stdout (layoutSmart defaultLayoutOptions { layoutPageWidth = AvailablePerLine s 0.8 } (doc <> line)))
    k
  eff (R other) = ReadlineC (eff (R (handleCoercible other)))


-- | This exists to work around the 'MonadException' constraint that haskeline entails.
newtype UnliftIOToMonadException m a = UnliftIOToMonadException { runUnliftIOToMonadException :: m a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (UnliftIOToMonadException m) where
  withRunInIO inner = UnliftIOToMonadException $ withRunInIO $ \ go -> inner (go . runUnliftIOToMonadException)

instance MonadUnliftIO m => MonadException (UnliftIOToMonadException m) where
  controlIO f = withRunInIO (\ runInIO -> f (RunIO (fmap pure . runInIO)) >>= runInIO)


newtype Line = Line Int

increment :: Line -> Line
increment (Line n) = Line (n + 1)

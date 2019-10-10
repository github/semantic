{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Readline.Haskeline
( -- * Readline effect
  module Control.Effect.Readline
  -- * Readline carrier
, runReadline
, runReadlineWithHistory
, ReadlineC (..)
, ControlIOC (..)
  -- * Re-exports
, Carrier
, run
, runM
) where

import Prelude hiding (print)

import Control.Effect.Carrier
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.Readline hiding (Carrier)
import Control.Monad.Fix
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Data.Text.Prettyprint.Doc.Render.Text
import System.Console.Haskeline hiding (Handler, handle)
import System.Directory
import System.FilePath

runReadline :: MonadException m => Prefs -> Settings m -> ReadlineC m a -> m a
runReadline prefs settings = runInputTWithPrefs prefs settings . runM . runReader (Line 0) . runReadlineC

runReadlineWithHistory :: MonadException m => ReadlineC m a -> m a
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

newtype ReadlineC m a = ReadlineC { runReadlineC :: ReaderC Line (LiftC (InputT m)) a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO)

instance (MonadException m, MonadIO m) => Carrier (Readline :+: Lift (InputT m)) (ReadlineC m) where
  eff (L (Prompt prompt k)) = ReadlineC $ do
    str <- lift (lift (getInputLine (cyan <> prompt <> plain)))
    Line line <- ask
    local increment (runReadlineC (k line str))
    where cyan = "\ESC[1;36m\STX"
          plain = "\ESC[0m\STX"
  eff (L (Print doc k)) = liftIO (putDoc doc) *> k
  eff (R other) = ReadlineC (eff (R (handleCoercible other)))


-- | This exists to work around the 'MonadException' constraint that haskeline entails.
newtype ControlIOC m a = ControlIOC { runControlIO :: m a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (ControlIOC m) where
  withRunInIO inner = ControlIOC $ withRunInIO $ \ go -> inner (go . runControlIO)

instance Carrier sig m => Carrier sig (ControlIOC m) where
  eff op = ControlIOC (eff (handleCoercible op))

instance (Carrier sig m, MonadUnliftIO m) => MonadException (ControlIOC m) where
  controlIO f = withRunInIO (\ runInIO -> f (RunIO (fmap pure . runInIO)) >>= runInIO)


newtype Line = Line Int

increment :: Line -> Line
increment (Line n) = Line (n + 1)

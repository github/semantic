{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, RankNTypes, TypeApplications, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Readline.Haskeline
( runReadline
, runReadlineWithHistory
, ReadlineC (..)
, runControlIO
, ControlIOC (..)
) where

import Control.Carrier
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Readline
import Control.Monad.IO.Class
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
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadException m, MonadIO m) => Carrier (Readline :+: Lift (InputT m)) (ReadlineC m) where
  eff (L (Prompt prompt k)) = ReadlineC $ do
    str <- lift (lift (getInputLine (cyan <> prompt <> plain)))
    local increment (runReadlineC (k str))
    where cyan = "\ESC[1;36m\STX"
          plain = "\ESC[0m\STX"
  eff (L (Print text k)) = liftIO (putDoc (unAnyDoc text)) *> k
  eff (L (AskLine k)) = ReadlineC ask >>= k
  eff (R other) = ReadlineC (eff (R (handleCoercible other)))


runControlIO :: (forall x . m x -> IO x) -> ControlIOC m a -> m a
runControlIO handler = runReader (Handler handler) . runControlIOC

-- | This exists to work around the 'MonadException' constraint that haskeline entails.
newtype ControlIOC m a = ControlIOC { runControlIOC :: ReaderC (Handler m) m a }
  deriving (Applicative, Functor, Monad, MonadIO)

newtype Handler m = Handler (forall x . m x -> IO x)

runHandler :: Handler m -> ControlIOC m a -> IO a
runHandler h@(Handler handler) = handler . runReader h . runControlIOC

instance Carrier sig m => Carrier sig (ControlIOC m) where
  eff op = ControlIOC (eff (R (handleCoercible op)))

instance (Carrier sig m, MonadIO m) => MonadException (ControlIOC m) where
  controlIO f = ControlIOC $ do
    handler <- ask
    liftIO (f (RunIO (fmap pure . runHandler handler)) >>= runHandler handler)

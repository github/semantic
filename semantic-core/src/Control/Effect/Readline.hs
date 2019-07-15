{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, OverloadedStrings, RankNTypes, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}

module Control.Effect.Readline
( Readline (..)
, prompt
, print
, println
, askLine
, Line (..)
, increment
, ReadlineC (..)
, runReadline
, runReadlineWithHistory
, TransC (..)
, ControlIOC (..)
, runControlIO
) where

import Prelude hiding (print)

import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Int
import Data.String
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import GHC.Generics (Generic1)
import System.Console.Haskeline hiding (Handler, handle)
import System.Directory
import System.FilePath

data Readline (m :: * -> *) k
  = Prompt String (Maybe String -> m k)
  | Print AnyDoc (m k)
  | AskLine (Line -> m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (Effect, HFunctor)

newtype AnyDoc = AnyDoc { unAnyDoc :: forall a . Doc a }

prompt :: (IsString str, Member Readline sig, Carrier sig m) => String -> m (Maybe str)
prompt p = fmap fromString <$> send (Prompt p pure)

print :: (Pretty a, Carrier sig m, Member Readline sig) => a -> m ()
print s = send (Print (AnyDoc (pretty s)) (pure ()))

println :: (Pretty a, Carrier sig m, Member Readline sig) => a -> m ()
println s = print s >> print @String "\n"

askLine :: (Carrier sig m, Member Readline sig) => m Line
askLine = send (AskLine pure)

newtype Line = Line Int64

increment :: Line -> Line
increment (Line n) = Line (n + 1)

newtype ReadlineC m a = ReadlineC { runReadlineC :: ReaderC Line (TransC InputT m) a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

runReadline :: MonadException m => Prefs -> Settings m -> ReadlineC m a -> m a
runReadline prefs settings = runInputTWithPrefs prefs settings . runTransC . runReader (Line 0) . runReadlineC

instance (Carrier sig m, Effect sig, MonadException m, MonadIO m) => Carrier (Readline :+: sig) (ReadlineC m) where
  eff (L (Prompt prompt k)) = ReadlineC $ do
    str <- lift (TransC (getInputLine (cyan <> prompt <> plain)))
    local increment (runReadlineC (k str))
    where cyan = "\ESC[1;36m\STX"
          plain = "\ESC[0m\STX"
  eff (L (Print text k)) = liftIO (putDoc (unAnyDoc text)) *> k
  eff (L (AskLine k)) = ReadlineC ask >>= k
  eff (R other) = ReadlineC (eff (R (handleCoercible other)))

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

-- | Promote a monad transformer into an effect.
newtype TransC t (m :: * -> *) a = TransC { runTransC :: t m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance (Carrier sig m, Effect sig, Monad (t m), MonadTrans t) => Carrier sig (TransC t m) where
  eff = TransC . join . lift . eff . handle (pure ()) (pure . (runTransC =<<))

runControlIO :: (forall x . m x -> IO x) -> ControlIOC m a -> m a
runControlIO handler = runReader (Handler handler) . runControlIOC

-- | This exists to work around the 'MonadException' constraint that haskeline entails.
newtype ControlIOC m a = ControlIOC { runControlIOC :: ReaderC (Handler m) m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

newtype Handler m = Handler (forall x . m x -> IO x)

runHandler :: Handler m -> ControlIOC m a -> IO a
runHandler h@(Handler handler) = handler . runReader h . runControlIOC

instance Carrier sig m => Carrier sig (ControlIOC m) where
  eff op = ControlIOC (eff (R (handleCoercible op)))

instance (Carrier sig m, MonadIO m) => MonadException (ControlIOC m) where
  controlIO f = ControlIOC $ do
    handler <- ask
    liftIO (f (RunIO (fmap pure . runHandler handler)) >>= runHandler handler)

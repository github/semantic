{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, TypeOperators, UndecidableInstances #-}

module Control.Effect.REPL
  ( REPL (..)
  , REPLC (..)
  , prompt
  , output
  , runREPL
  ) where

import Prologue

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Effect.Reader
import System.Console.Haskeline
import qualified Data.Text as T

data REPL (m :: * -> *) k
  = Prompt Text (Maybe Text -> k)
  | Output Text k
  deriving (Functor)

instance HFunctor REPL where
  hmap _ = coerce

instance Effect REPL where
  handle state handler (Prompt p k) = Prompt p (handler . (<$ state) . k)
  handle state handler (Output s k) = Output s (handler (k <$ state))

prompt :: (Member REPL sig, Carrier sig m) => Text -> m (Maybe Text)
prompt p = send (Prompt p pure)

output :: (Member REPL sig, Carrier sig m) => Text -> m ()
output s = send (Output s (pure ()))

runREPL :: Prefs -> Settings IO -> REPLC m a -> m a
runREPL prefs settings = runReader (prefs, settings) . runREPLC

newtype REPLC m a = REPLC { runREPLC :: ReaderC (Prefs, Settings IO) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Carrier sig m, MonadIO m) => Carrier (REPL :+: sig) (REPLC m) where
  eff (L op) = do
    args <- REPLC ask
    case op of
      Prompt p k -> liftIO (uncurry runInputTWithPrefs args (fmap (fmap T.pack) (getInputLine (cyan <> T.unpack p <> plain)))) >>= k
      Output s k -> liftIO (uncurry runInputTWithPrefs args (outputStrLn (T.unpack s))) *> k
  eff (R other) = REPLC (eff (R (handleCoercible other)))


cyan :: String
cyan = "\ESC[1;36m\STX"

plain :: String
plain = "\ESC[0m\STX"

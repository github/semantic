{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Control.Effect.REPL
  ( REPL (..)
  , REPLC (..)
  , prompt
  , output
  , runREPL
  ) where

import Prologue

import Control.Algebra
import Control.Carrier.Reader
import System.Console.Haskeline
import qualified Data.Text as T

data REPL (m :: * -> *) k
  = Prompt Text (Maybe Text -> m k)
  | Output Text (m k)
  deriving (Functor, Generic1)

instance HFunctor REPL
instance Effect   REPL

prompt :: Has REPL sig m => Text -> m (Maybe Text)
prompt p = send (Prompt p pure)

output :: Has REPL sig m => Text -> m ()
output s = send (Output s (pure ()))

runREPL :: Prefs -> Settings IO -> REPLC m a -> m a
runREPL prefs settings = runReader (prefs, settings) . runREPLC

newtype REPLC m a = REPLC { runREPLC :: ReaderC (Prefs, Settings IO) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (REPL :+: sig) (REPLC m) where
  alg (L op) = do
    args <- REPLC ask
    case op of
      Prompt p k -> liftIO (uncurry runInputTWithPrefs args (fmap (fmap T.pack) (getInputLine (cyan <> T.unpack p <> plain)))) >>= k
      Output s k -> liftIO (uncurry runInputTWithPrefs args (outputStrLn (T.unpack s))) *> k
  alg (R other) = REPLC (alg (R (handleCoercible other)))


cyan :: String
cyan = "\ESC[1;36m\STX"

plain :: String
plain = "\ESC[0m\STX"

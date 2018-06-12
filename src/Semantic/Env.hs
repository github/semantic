module Semantic.Env where

import Control.Monad.IO.Class
import Prologue
import System.Environment
import Text.Read (readMaybe)

envLookupHost :: MonadIO io => String -> String -> io String
envLookupHost defaultHost k = liftIO $ fromMaybe defaultHost <$> lookupEnv k

envLookupPort :: MonadIO io => Int -> String -> io Int
envLookupPort defaultPort k = liftIO $ parsePort <$> lookupEnv k
  where parsePort x | Just s <- x
                    , Just p <- readMaybe s = p
                    | otherwise = defaultPort

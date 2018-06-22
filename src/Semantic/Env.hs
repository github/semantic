module Semantic.Env where

import Control.Monad.IO.Class
import Prologue
import System.Environment
import Text.Read (readMaybe)

envLookupString :: MonadIO io => String -> String -> io String
envLookupString defaultVal k = liftIO $ fromMaybe defaultVal <$> lookupEnv k

envLookupInt :: MonadIO io => Int -> String -> io Int
envLookupInt defaultVal k = liftIO $ parse <$> lookupEnv k
  where parse x | Just s <- x
                    , Just p <- readMaybe s = p
                    | otherwise = defaultVal

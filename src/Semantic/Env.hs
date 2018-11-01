{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Semantic.Env
  ( envLookupNum
  , envLookupString
  ) where

import Prologue
import System.Environment
import Text.Read (readMaybe)

envLookupString :: MonadIO io => String -> String -> io String
envLookupString defaultVal k = liftIO $ fromMaybe defaultVal <$> lookupEnv k

-- | Although the `Num a` constraint is redundant (hence -Wno-redundant-constraints), we use this constraint to communicate this function is meant to read Num values.
envLookupNum :: (MonadIO io, Num a, Read a) => a -> String -> io a
envLookupNum defaultVal k = liftIO $ parse <$> lookupEnv k
  where parse x | Just s <- x, Just p <- readMaybe s = p
                | otherwise = defaultVal

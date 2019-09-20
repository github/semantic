module Main
( main
) where

import System.Environment
import Test.DocTest

main :: IO ()
main = do
  args <- getArgs
  autogen <- fmap (<> "/build/doctest/autogen") <$> lookupEnv "HASKELL_DIST_DIR"
  doctest (maybe id ((:) . ("-i" <>)) autogen ("-isemantic-source/src" : "--fast" : if null args then ["semantic-source/src"] else args))

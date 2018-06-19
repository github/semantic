module Main (main) where

import Language.Haskell.HLint (Severity(..), hlint, suggestionSeverity)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    hints <- hlint [ "src" ]
    if null (filter ((>= Warning) . suggestionSeverity) hints) then exitSuccess else exitFailure

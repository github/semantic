module Semantic.Util where

import Data.Blob
import Language.Haskell.HsColour (hscolour, Output(TTY))
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import Text.Show.Pretty (ppShow)
import Files

-- Produces colorized pretty-printed output for the terminal / GHCi.
pp :: Show a => a -> IO ()
pp = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow

file :: FilePath -> IO Blob
file path = Files.readFile path (languageForFilePath path)

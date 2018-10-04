module Semantic.Util.Pretty (prettyShow) where

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Text.Show.Pretty (ppShow)

prettyShow :: Show a => a -> IO ()
prettyShow = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow

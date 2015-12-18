module PatchOutput where

import Diff

patch :: Diff a Info -> String -> String -> String
patch diff sourceA sourceB = mconcat $ hunks diff sourceA sourceB

hunks :: Diff a Info -> String -> String -> [String]
hunks diff sourceA sourceB = []

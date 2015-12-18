module PatchOutput where

import Diff

patch :: Diff a Info -> String -> String -> String
patch diff sourceA sourceB = mconcat $ hunks diff sourceA sourceB

data Hunk = Hunk [Line]
data Line = Insert String | Delete String | Context String

hunks :: Diff a Info -> String -> String -> [String]
hunks diff sourceA sourceB = []

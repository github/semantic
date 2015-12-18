module PatchOutput where

import Diff

patch :: Diff a Info -> String -> String -> String
patch diff sourceA sourceB = mconcat $ hunks diff sourceA sourceB

data Hunk = Hunk Int Int [Line]

header :: Hunk -> String
header (Hunk offsetA offsetB _) = "@@ -" ++ show offsetA ++ " +" ++ show offsetB ++ " @@\n"

data Line = Insert String | Delete String | Context String

hunks :: Diff a Info -> String -> String -> [String]
hunks diff sourceA sourceB = []

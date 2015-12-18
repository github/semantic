module PatchOutput where

import Diff

patch :: Diff a Info -> String -> String -> String
patch diff sourceA sourceB = mconcat $ hunks diff sourceA sourceB

data Hunk = Hunk Int Int [Line]

header :: Hunk -> String
header (Hunk offsetA offsetB lines) = "@@ -" ++ show offsetA ++ "," ++ show countDeleted ++ " +" ++ show offsetB ++ "," ++ show countInserted ++ " @@\n"
  where (countDeleted, countInserted) = foldl countLine (0 :: Int, 0 :: Int) lines
        countLine (countDeleted, countInserted) line = case line of
          Insert _ -> (countDeleted, countInserted + 1)
          Delete _ -> (countDeleted + 1, countInserted)
          Context _ -> (countDeleted + 1, countInserted + 1)

data Line = Insert String | Delete String | Context String

hunks :: Diff a Info -> String -> String -> [String]
hunks diff sourceA sourceB = []

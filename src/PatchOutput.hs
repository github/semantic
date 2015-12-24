module PatchOutput (
  patch,
  hunks
) where

import Diff
import Source hiding ((++))
import Control.Monad.Free

patch :: Diff a Info -> Source Char -> Source Char -> String
patch diff sourceA sourceB = mconcat $ show <$> hunks diff sourceA sourceB

data Hunk = Hunk Int Int [Line]
  deriving Eq

instance Show Hunk where
  show = header

header :: Hunk -> String
header (Hunk offsetA offsetB lines) = "@@ -" ++ show offsetA ++ "," ++ show countDeleted ++ " +" ++ show offsetB ++ "," ++ show countInserted ++ " @@\n"
  where (countDeleted, countInserted) = foldl countLine (0 :: Int, 0 :: Int) lines
        countLine (countDeleted, countInserted) line = case line of
          Insert _ -> (countDeleted, countInserted + 1)
          Delete _ -> (countDeleted + 1, countInserted)
          Context _ -> (countDeleted + 1, countInserted + 1)

data Line = Insert String | Delete String | Context String
  deriving (Show, Eq)

hunks :: Diff a Info -> Source Char -> Source Char -> [Hunk]
hunks diff sourceA sourceB = case diff of
  Pure patch -> []
  Free (Annotated (a, b) syntax) -> []

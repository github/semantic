module PatchOutput (
  patch,
  hunks
) where

import Diff
import Source hiding ((++))
import Control.Monad.Free

patch :: Diff a Info -> Source Char -> Source Char -> String
patch diff sourceA sourceB = mconcat $ show <$> hunks diff sourceA sourceB

data Hunk = Hunk { offsetA :: Int, offsetB :: Int, leadingContext :: [String], deletions :: [String], insertions :: [String] , trailingContext :: [String] }
  deriving Eq

instance Show Hunk where
  show = header

header :: Hunk -> String
header hunk = "@@ -" ++ show (offsetA hunk) ++ "," ++ show (length $ deletions hunk) ++ " +" ++ show (offsetB hunk) ++ "," ++ show (length $ insertions hunk) ++ " @@\n"

hunks :: Diff a Info -> Source Char -> Source Char -> [Hunk]
hunks diff sourceA sourceB = case diff of
  Pure patch -> []
  Free (Annotated (a, b) syntax) -> []

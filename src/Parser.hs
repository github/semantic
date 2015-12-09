module Parser where

import Diff
import Term

type Parser = String -> IO (Term String Info)

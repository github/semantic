module Parser where

import Diff
import Term

type Parser = String -> IO (Term String Info)

-- | Given a source string and a term’s annotation & production/child pairs, construct the term.
type Constructor = String -> Info -> [(String, Term String Info)] -> Term String Info

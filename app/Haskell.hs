module Haskell where

import Text.Parsec
import Text.Parsec.String

haskellParser :: Parser String
haskellParser = many anyChar

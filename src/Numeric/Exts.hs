{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Numeric.Exts
  ( parseInteger
  , hex
  , oct
  , bin
  , attempt
  , lengths
  ) where

import Control.Applicative
import Control.Monad hiding (fail)
import Data.Attoparsec.Text
import Data.Char (isDigit, isOctDigit, isHexDigit)
import Data.Text
import Numeric
import Prelude hiding (fail, filter)
import Prologue
import Text.Read (readMaybe)

-- The ending stanza. Note the explicit endOfInput call to ensure we haven't left any dangling input.
lengths :: Parser ()
lengths = skipWhile (inClass "iIjJlL") *> endOfInput

stripUnder :: Text -> Text
stripUnder = filter (/= '_')

-- Parse a hex value, leaning on the parser provided by Attoparsec.
hex :: Num a => Parser a
hex = do
  void (char '0')
  skip (inClass "xX")
  let isHex c = isHexDigit c || (c == '_')
  contents <- stripUnder <$> takeWhile1 isHex
  let go = fromIntegral <$> hexadecimal @Integer <* lengths
  either fail pure (parseOnly go contents)

-- We lean on Haskell's octal integer support, parsing
-- the given string as an integer then coercing it to a Scientific.
oct :: Num a => Parser a
oct = do
  void (char '0')
  skipWhile (inClass "Oo")
  let isOct c = isOctDigit c || c == '_'
  digs <- stripUnder <$> (takeWhile1 isOct <* lengths)
  fromIntegral <$> attempt @Integer (unpack ("0o" <> digs)) <* lengths

-- The case for binary literals is somewhat baroque. Despite having binary literal support, Integer's
-- Read instance does not handle binary literals. So we have to shell out to Numeric.readInt, which
-- is a very strange API, but works for our use case. The use of 'error' looks partial, but if Attoparsec
-- and readInt do their jobs, it should never happen.
bin :: (Show a, Num a) => Parser a
bin = do
  void (char '0')
  skip (inClass "bB")
  let isBin = inClass "01_"
  digs <- unpack . stripUnder <$> (takeWhile1 isBin <* lengths)
  let c2b c = case c of
        '0' -> 0
        '1' -> 1
        x   -> error ("Invariant violated: both Attoparsec and readInt let a bad digit through: " <> [x])
  let res = readInt 2 isBin c2b digs
  case res of
    []        -> fail ("No parse of binary literal: " <> digs)
    [(x, "")] -> x <$ lengths
    others    -> fail ("Too many parses of binary literal: " <> show others)

-- Wrapper around readMaybe.
attempt :: Read a => String -> Parser a
attempt str = maybeM (fail ("No parse: " <> str)) (readMaybe str)

parseInteger :: Text -> Either String Integer
parseInteger = parseOnly integerParser

-- | A simplified version of parseScientific.
integerParser :: Parser Integer
integerParser = signed (choice [hex, oct, bin, dec]) where
  dec = do
    let notUnder = filter (/= '_')
    let decOrUnder c = isDigit c || (c == '_')
    contents <- notUnder <$> takeWhile1 decOrUnder
    void lengths
    attempt (unpack contents)

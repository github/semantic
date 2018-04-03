module Data.Scientific.Exts
    ( module Data.Scientific
    , parseScientific
    , parser
    ) where

import Prelude hiding (filter, null, takeWhile)

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 hiding (readInt, takeWhile)
import Data.Char (isOctDigit)
import Data.Scientific
import Data.Semigroup
import Numeric
import Text.Read (readMaybe)

parseScientific :: ByteString -> Either String Scientific
parseScientific = parseOnly parser

-- | This is a very flexible and forgiving parser for Scientific values.
-- Unlike 'scientificP' or Scientific's 'Read' instance, this handles the myriad
-- array of floating-point syntaxes across languages:
-- * omitted whole parts, e.g. @.5@
-- * omitted decimal parts, e.g. @5.@
-- * numbers with trailing imaginary/length specifiers, @1.7j, 20L@
-- * numeric parts, in whole or decimal or exponent parts, with @_@ characters
-- * hexadecimal, octal, and binary literals (TypeScript needs this because all numbers are floats)
-- You may either omit the whole or the leading part, not both; this parser also rejects the empty string.
-- It does /not/ handle hexadecimal floating-point numbers yet, as no language we parse supports them.
-- This will need to be changed when we support Java.
-- Please note there are extant parser bugs where complex literals (e.g. @123j@) are parsed
-- as floating-point rather than complex quantities. This parser discards all suffixes.
-- This parser is unit-tested in Data.Scientific.Spec.

parser :: Parser Scientific
parser = signed (choice [hex, oct, bin, dec]) where
  -- The ending stanza. Note the explicit endOfInput call to ensure we haven't left any dangling input.
  done = skipWhile (inClass "iIjJlL") *> endOfInput

  -- Wrapper around readMaybe. Analogous to maybeFail in the Prologue, but no need to pull that in.
  attempt :: Read a => String -> Parser a
  attempt str = maybe (fail ("No parse: " <> str)) pure (readMaybe str)

  -- Parse a hex value, leaning on the parser provided by Attoparsec.
  hex = fromIntegral <$> (string "0x" *> hexadecimal @Integer)

  -- Here's where things start getting icky. We lean on Haskell's octal integer support, parsing
  -- the given string as an integer then coercing it to a Scientific.
  oct = do
    void (char '0' <* optional (char 'o'))
    digs <- takeWhile1 isOctDigit <* done
    fromIntegral <$> attempt @Integer (unpack ("0o" <> digs))

  -- This is where it starts getting really horrible. Despite having binary literal support, Integer's
  -- Read instance does not handle binary literals. So we have to shell out to Numeric.readInt, which is
  -- in all respects a miserable excuse for an API, and apply a ReadS manually. This sucks so much.
  -- The use of 'error' looks partial, but I really promise you it isn't.
  bin = do
    void (string "0b")
    let isBin = inClass "01"
    digs <- unpack <$> (takeWhile1 isBin <* done)
    let c2b c = case c of
          '0' -> 0
          '1' -> 1
          x   -> error ("Invariant violated: both Attoparsec and readInt let a bad digit through: " <> [x])
    let res = readInt 2 isBin c2b digs
    case res of
      []        -> fail ("No parse of binary literal: " <> digs)
      [(x, "")] -> pure x
      others    -> fail ("Too many parses of binary literal: " <> show others)

  -- Compared to the binary parser, this is positively breezy.
  dec = do
    let notUnder = filter (/= '_')
    let decOrUnder c = isDigit c || (c == '_')

    -- Try getting the whole part of a floating literal.
    leadings <- notUnder <$> takeWhile decOrUnder

    -- Try reading a dot.
    void (optional (char '.'))

    -- The trailing part...
    trailings <- notUnder <$> takeWhile decOrUnder

    -- ...and the exponent.
    exponent  <- notUnder <$> takeWhile (inClass "eE_0123456789+-")

    -- Ensure we don't read an empty string, or one consisting only of a dot and/or an exponent.
    when (null trailings && null leadings) (fail "Does not accept a single dot")

    -- Replace empty parts with a zero.
    let leads = if null leadings then "0" else leadings
    let trail = if null trailings then "0" else trailings

    attempt (unpack (leads <> "." <> trail <> exponent))

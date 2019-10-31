{-# LANGUAGE OverloadedStrings #-}
module Data.Scientific.Exts
    ( module Data.Scientific
    , attemptUnsafeArithmetic
    , parseScientific
    ) where

import Control.Applicative
import Control.Exception as Exc (evaluate, try)
import Control.Monad hiding (fail)
import Data.Attoparsec.Text
import Data.Text hiding (takeWhile)
import Data.Char (isDigit)
import Data.Scientific
import Prelude hiding (fail, filter, null, takeWhile)
import Prologue hiding (null)
import System.IO.Unsafe
import Numeric.Exts

parseScientific :: Text -> Either String Scientific
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

    lengths

    -- Ensure we don't read an empty string, or one consisting only of a dot and/or an exponent.
    when (null trailings && null leadings) (fail "Does not accept a single dot")

    -- Replace empty parts with a zero.
    let leads = if null leadings then "0" else leadings
    let trail = if null trailings then "0" else trailings

    attempt (unpack (leads <> "." <> trail <> exponent))

-- | Attempt to evaluate the given term into WHNF. If doing so raises an 'ArithException', such as
-- 'ZeroDivisionError' or 'RatioZeroDenominator', 'Left' will be returned.
-- Hooray for uncatchable exceptions that bubble up from third-party code.
attemptUnsafeArithmetic :: a -> Either ArithException a
attemptUnsafeArithmetic = unsafePerformIO . Exc.try . evaluate
{-# NOINLINE attemptUnsafeArithmetic #-}

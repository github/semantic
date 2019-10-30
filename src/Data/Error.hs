{-# LANGUAGE DeriveFunctor, FlexibleInstances, GADTs, RankNTypes, RecordWildCards #-}
module Data.Error
  ( Error (..)
  , formatError
  , makeError
  , showExpectation
  , showExcerpt
  , withSGRCode
  -- * Flags affecting 'Error' values
  , LogPrintSource (..)
  , Colourize (..)
  ) where

import Prologue

import Data.ByteString.Char8 (unpack)
import Data.Ix (inRange)
import Data.List (intersperse, isSuffixOf)
import System.Console.ANSI

import           Data.Blob
import           Data.Flag as Flag
import qualified Source.Source as Source
import           Source.Span

data LogPrintSource = LogPrintSource
data Colourize = Colourize

-- | Rather than using the Error constructor directly, you probably
-- want to call 'makeError', which takes care of inserting the call
-- stack for you.
data Error grammar = Error
  { errorSpan      :: Span
  , errorExpected  :: [grammar]
  , errorActual    :: Maybe grammar
  , errorCallStack :: CallStack
  } deriving (Show, Functor)

-- | This instance does not take into account the call stack.
instance Eq grammar => Eq (Error grammar) where
  (Error s e a _) == (Error s' e' a' _) = (s == s') && (e == e') && (a == a')

instance Exception (Error String)

makeError :: HasCallStack => Span -> [grammar] -> Maybe grammar -> Error grammar
makeError s e a = withFrozenCallStack (Error s e a callStack)

-- | Format an 'Error', optionally with reference to the source where it occurred.
formatError :: Flag LogPrintSource -> Flag Colourize -> Blob -> Error String -> String
formatError includeSource colourize blob@Blob{..} Error{..}
  = ($ "")
  $ withSGRCode colourize [SetConsoleIntensity BoldIntensity] (showSpan path errorSpan . showString ": ")
  . withSGRCode colourize [SetColor Foreground Vivid Red] (showString "error") . showString ": " . showExpectation colourize errorExpected errorActual . showChar '\n'
  . (if Flag.toBool LogPrintSource includeSource then showExcerpt colourize errorSpan blob else id)
  . showCallStack colourize callStack . showChar '\n'
  where
    path = Just $ if Flag.toBool LogPrintSource includeSource then blobPath blob else "<filtered>"

showExcerpt :: Flag Colourize -> Span -> Blob -> ShowS
showExcerpt colourize Span{..} Blob{..}
  = showString context . (if "\n" `isSuffixOf` context then id else showChar '\n')
  . showString (replicate (caretPaddingWidth + lineNumberDigits) ' ') . withSGRCode colourize [SetColor Foreground Vivid Green] (showString caret) . showChar '\n'
  where context = fold contextLines
        contextLines = [ showLineNumber i <> ": " <> unpack (Source.bytes l)
                       | (i, l) <- zip [1..] (Source.lines blobSource)
                       , inRange (line start - 2, line start) i
                       ]
        showLineNumber n = let s = show n in replicate (lineNumberDigits - length s) ' ' <> s
        lineNumberDigits = succ (floor (logBase 10 (fromIntegral (line start) :: Double)))
        caretPaddingWidth = succ (column start)
        caret | line start == line end = replicate (max 1 (column end - column start)) '^'
              | otherwise                            = "^..."

withSGRCode :: Flag Colourize -> [SGR] -> ShowS -> ShowS
withSGRCode useColour code content
  | Flag.toBool Colourize useColour = showString (setSGRCode code) . content . showString (setSGRCode [])
  | otherwise = content

showExpectation :: Flag Colourize -> [String] -> Maybe String -> ShowS
showExpectation colourize = go
  where go [] Nothing = showString "no rule to match at " . showActual "end of branch"
        go expected Nothing = showString "expected " . showSymbols colourize expected . showString " at " . showActual "end of branch"
        go expected (Just actual) = showString "expected " . showSymbols colourize expected . showString ", but got " . showActual actual
        showActual = withSGRCode colourize [SetColor Foreground Vivid Green] . showString

showSymbols :: Flag Colourize -> [String] -> ShowS
showSymbols colourize = go
  where go []        = showString "end of input nodes"
        go [symbol]  = showSymbol symbol
        go [a, b]    = showSymbol a . showString " or " . showSymbol b
        go [a, b, c] = showSymbol a . showString ", " . showSymbol b . showString ", or " . showSymbol c
        go (h:t)     = showSymbol h . showString ", " . go t
        showSymbol = withSGRCode colourize [SetColor Foreground Vivid Red] . showString

showSpan :: Maybe FilePath -> Span -> ShowS
showSpan path Span{..} = maybe (showParen True (showString "interactive")) showString path . showChar ':' . (if start == end then showPos start else showPos start . showChar '-' . showPos end)
  where showPos Pos{..} = shows line . showChar ':' . shows column

showCallStack :: Flag Colourize -> CallStack -> ShowS
showCallStack colourize callStack = foldr (.) id (intersperse (showChar '\n') (uncurry (showCallSite colourize) <$> getCallStack callStack))

showCallSite :: Flag Colourize -> String -> SrcLoc -> ShowS
showCallSite colourize symbol loc@SrcLoc{..} = showString symbol . showChar ' ' . withSGRCode colourize [SetConsoleIntensity BoldIntensity] (showParen True (showSpan (Just srcLocFile) (spanFromSrcLoc loc)))

{-# LANGUAGE GADTs, ImplicitParams, RankNTypes, StandaloneDeriving #-}
module Data.Error where

import Data.Blob
import Data.ByteString (isSuffixOf)
import Data.ByteString.Char8 (pack, unpack)
import Data.Ix (inRange)
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup
import Data.Source
import Data.Span
import GHC.Stack
import System.Console.ANSI

data Error grammar = HasCallStack => Error { errorSpan :: Span, errorExpected :: [grammar], errorActual :: Maybe grammar }

deriving instance Eq grammar => Eq (Error grammar)
deriving instance Foldable Error
deriving instance Functor Error
deriving instance Show grammar => Show (Error grammar)
deriving instance Traversable Error

errorCallStack :: Error grammar -> CallStack
errorCallStack Error{} = callStack


withCallStack :: CallStack -> (HasCallStack => a) -> a
withCallStack cs action = let ?callStack = cs in action

type IncludeSource = Bool
type Colourize = Bool

-- | Format an 'Error', optionally with reference to the source where it occurred.
formatError :: IncludeSource -> Colourize -> Blob -> Error String -> String
formatError includeSource colourize Blob{..} Error{..}
  = ($ "")
  $ withSGRCode colourize [SetConsoleIntensity BoldIntensity] (showSpan (maybe Nothing (const (Just blobPath)) blobKind) errorSpan . showString ": ")
  . withSGRCode colourize [SetColor Foreground Vivid Red] (showString "error") . showString ": " . showExpectation colourize errorExpected errorActual . showChar '\n'
  . (if includeSource
    then showString (unpack context) . (if "\n" `isSuffixOf` context then id else showChar '\n')
       . showString (replicate (succ (posColumn (spanStart errorSpan) + lineNumberDigits)) ' ') . withSGRCode colourize [SetColor Foreground Vivid Green] (showChar '^' . showChar '\n')
    else id)
  . showString (prettyCallStack callStack) . showChar '\n'
  where context = maybe "\n" (sourceBytes . sconcat) (nonEmpty [ fromBytes (pack (showLineNumber i)) <> fromBytes ": " <> l | (i, l) <- zip [1..] (sourceLines blobSource), inRange (posLine (spanStart errorSpan) - 2, posLine (spanStart errorSpan)) i ])
        showLineNumber n = let s = show n in replicate (lineNumberDigits - length s) ' ' <> s
        lineNumberDigits = succ (floor (logBase 10 (fromIntegral (posLine (spanStart errorSpan)) :: Double)))

withSGRCode :: Colourize -> [SGR] -> ShowS -> ShowS
withSGRCode useColour code content =
  if useColour then
    showString (setSGRCode code)
    . content
    . showString (setSGRCode [])
  else
    content

showExpectation :: Colourize -> [String] -> Maybe String -> ShowS
showExpectation colourize = go
  where go [] Nothing = showString "no rule to match at " . showActual "end of input nodes"
        go expected Nothing = showString "expected " . showSymbols colourize expected . showString " at " . showActual "end of input nodes"
        go expected (Just actual) = showString "expected " . showSymbols colourize expected . showString ", but got " . showActual actual
        showActual = withSGRCode colourize [SetColor Foreground Vivid Green] . showString

showSymbols :: Colourize -> [String] -> ShowS
showSymbols colourize = go
  where go [] = showString "end of input nodes"
        go [symbol] = showSymbol symbol
        go [a, b] = showSymbol a . showString " or " . showSymbol b
        go [a, b, c] = showSymbol a . showString ", " . showSymbol b . showString ", or " . showSymbol c
        go (h:t) = showSymbol h . showString ", " . go t
        showSymbol = withSGRCode colourize [SetColor Foreground Vivid Red] . showString

showSpan :: Maybe FilePath -> Span -> ShowS
showSpan path Span{..} = maybe (showParen True (showString "interactive")) showString path . showChar ':' . (if spanStart == spanEnd then showPos spanStart else showPos spanStart . showChar '-' . showPos spanEnd)
  where showPos Pos{..} = shows posLine . showChar ':' . shows posColumn

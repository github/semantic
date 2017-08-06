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
  . withSGRCode colourize [SetColor Foreground Vivid Red] (showString "error" . showString ": " . showExpectation errorExpected errorActual . showChar '\n')
  . (if includeSource
    then showString (unpack context) . (if "\n" `isSuffixOf` context then id else showChar '\n')
       . showString (replicate (succ (posColumn (spanStart errorSpan) + lineNumberDigits)) ' ') . withSGRCode colourize [SetColor Foreground Vivid Green] (showChar '^' . showChar '\n')
    else id)
  . showString (prettyCallStack callStack) . showChar '\n'
  where context = maybe "\n" (sourceBytes . sconcat) (nonEmpty [ fromBytes (pack (showLineNumber i)) <> fromBytes ": " <> l | (i, l) <- zip [1..] (sourceLines blobSource), inRange (posLine (spanStart errorSpan) - 2, posLine (spanStart errorSpan)) i ])
        showLineNumber n = let s = show n in replicate (lineNumberDigits - length s) ' ' <> s
        lineNumberDigits = succ (floor (logBase 10 (fromIntegral (posLine (spanStart errorSpan)) :: Double)))

withSGRCode :: Bool -> [SGR] -> ShowS -> ShowS
withSGRCode useColour code content =
  if useColour then
    showString (setSGRCode code)
    . content
    . showString (setSGRCode [])
  else
    content

showExpectation :: [String] -> Maybe String -> ShowS
showExpectation [] Nothing = showString "no rule to match at end of input nodes"
showExpectation expected Nothing = showString "expected " . showSymbols expected . showString " at end of input nodes"
showExpectation expected (Just actual) = showString "expected " . showSymbols expected . showString ", but got " . showString actual

showSymbols :: [String] -> ShowS
showSymbols [] = showString "end of input nodes"
showSymbols [symbol] = showString symbol
showSymbols [a, b] = showString a . showString " or " . showString b
showSymbols [a, b, c] = showString a . showString ", " . showString b . showString ", or " . showString c
showSymbols (h:t) = showString h . showString ", " . showSymbols t

showSpan :: Maybe FilePath -> Span -> ShowS
showSpan path Span{..} = maybe (showParen True (showString "interactive")) showString path . showChar ':' . (if spanStart == spanEnd then showPos spanStart else showPos spanStart . showChar '-' . showPos spanEnd)
  where showPos Pos{..} = shows posLine . showChar ':' . shows posColumn

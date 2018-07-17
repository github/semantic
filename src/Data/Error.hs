{-# LANGUAGE GADTs, ImplicitParams, RankNTypes, StandaloneDeriving #-}
module Data.Error where

import Prologue
import Data.Blob
import Data.ByteString (isSuffixOf)
import Data.ByteString.Char8 (pack, unpack)
import Data.Ix (inRange)
import Data.List (intersperse)
import Data.Source
import Data.Span
import System.Console.ANSI

data Error grammar = Error
  { errorSpan :: Span
  , errorExpected :: [grammar]
  , errorActual :: Maybe grammar
  , errorCallStack :: CallStack
  } deriving (Typeable)

-- | This instance does not take into account the call stack.
instance Eq grammar => Eq (Error grammar) where
  (Error s e a _) == (Error s' e' a' _) = (s == s') && (e == e') && (a == a')

deriving instance Foldable Error
deriving instance Functor Error
deriving instance Show grammar => Show (Error grammar)
deriving instance Traversable Error
instance Exception (Error String)

makeError :: HasCallStack => Span -> [grammar] -> Maybe grammar -> Error grammar
makeError s e a = Error s e a ?callStack

withCallStack :: CallStack -> (HasCallStack => a) -> a
withCallStack cs action = let ?callStack = cs in action

type IncludeSource = Bool
type Colourize = Bool

-- | Format an 'Error', optionally with reference to the source where it occurred.
formatError :: IncludeSource -> Colourize -> Blob -> Error String -> String
formatError includeSource colourize Blob{..} Error{..}
  = ($ "")
  $ withSGRCode colourize [SetConsoleIntensity BoldIntensity] (showSpan (Just blobPath) errorSpan . showString ": ")
  . withSGRCode colourize [SetColor Foreground Vivid Red] (showString "error") . showString ": " . showExpectation colourize errorExpected errorActual . showChar '\n'
  . (if includeSource
    then showString (unpack context) . (if "\n" `isSuffixOf` context then id else showChar '\n')
       . showString (replicate (succ (posColumn (spanStart errorSpan) + lineNumberDigits)) ' ') . withSGRCode colourize [SetColor Foreground Vivid Green] (showChar '^' . showChar '\n')
    else id)
  . showCallStack colourize callStack . showChar '\n'
  where context = maybe "\n" (sourceBytes . sconcat) (nonEmpty [ fromUTF8 (pack (showLineNumber i)) <> fromUTF8 ": " <> l | (i, l) <- zip [1..] (sourceLines blobSource), inRange (posLine (spanStart errorSpan) - 2, posLine (spanStart errorSpan)) i ])
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
  where go [] Nothing = showString "no rule to match at " . showActual "end of branch"
        go expected Nothing = showString "expected " . showSymbols colourize expected . showString " at " . showActual "end of branch"
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

showCallStack :: Colourize -> CallStack -> ShowS
showCallStack colourize callStack = foldr (.) id (intersperse (showChar '\n') (uncurry (showCallSite colourize) <$> getCallStack callStack))

showCallSite :: Colourize -> String -> SrcLoc -> ShowS
showCallSite colourize symbol loc@SrcLoc{..} = showString symbol . showChar ' ' . withSGRCode colourize [SetConsoleIntensity BoldIntensity] (showParen True (showSpan (Just srcLocFile) (spanFromSrcLoc loc)))

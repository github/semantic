{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeFamilies #-}
module Data.Syntax.Assignment
( Assignment
, Location
, location
, symbol
, source
, children
, Rose(..)
, RoseF(..)
, Node
, AST
, Result(..)
, Error(..)
, showError
, assign
, runAssignment
, AssignmentState(..)
, makeState
) where

import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Nil)
import qualified Data.IntMap.Lazy as IntMap
import Data.Ix (inRange)
import Data.List.NonEmpty (nonEmpty)
import Data.Record
import qualified Info
import Prologue hiding (Alt, get, Location, state)
import Range (offsetRange)
import qualified Source (Source(..), drop, slice, sourceText, actualLines)
import Text.Parser.TreeSitter.Language
import Text.Show hiding (show)

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment node = Freer (AssignmentF node)

data AssignmentF node a where
  Location :: AssignmentF node Location
  Source :: AssignmentF symbol ByteString
  Children :: Assignment symbol a -> AssignmentF symbol a
  Choose :: IntMap.IntMap a -> AssignmentF node a
  Alt :: a -> a -> AssignmentF symbol a
  Empty :: AssignmentF symbol a

-- | Zero-width production of the current location.
--
--   If assigning at the end of input or at the end of a list of children, the loccation will be returned as an empty Range and SourceSpan at the current offset. Otherwise, it will be the Range and SourceSpan of the current node.
location :: Assignment (Node grammar) Location
location = Location `Then` return

-- | Zero-width match of a node with the given symbol.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. 'many (symbol A *> b)' is fine, but 'many (symbol A)' is not.
symbol :: (Enum symbol, Eq symbol) => symbol -> Assignment (Node symbol) ()
symbol s = Choose (IntMap.singleton (fromEnum s) ()) `Then` return

-- | A rule to produce a node’s source as a ByteString.
source :: Assignment symbol ByteString
source = Source `Then` return

-- | Match a node by applying an assignment to its children.
children :: Assignment symbol a -> Assignment symbol a
children forEach = Children forEach `Then` return


-- | A rose tree.
data Rose a = Rose { roseValue :: !a, roseChildren :: ![Rose a] }
  deriving (Eq, Functor, Show)

-- | A location specified as possibly-empty intervals of bytes and line/column positions.
type Location = Record '[Info.Range, Info.SourceSpan]

-- | The label annotating a node in the AST, specified as the pairing of its symbol and location information.
type Node grammar = Record '[grammar, Info.Range, Info.SourceSpan]

-- | An abstract syntax tree in some 'grammar', with symbols and location information annotating each node.
type AST grammar = Rose (Node grammar)


-- | The result of assignment, possibly containing an error.
data Result symbol a = Result { resultErrors :: [Error symbol], resultValue :: Maybe a }
  deriving (Eq, Foldable, Functor, Traversable)

data Error symbol = Error
  { errorPos :: Info.SourcePos
  , errorExpected :: [symbol]
  , errorActual :: Maybe symbol
  }
  deriving (Eq, Show)

-- | Pretty-print an Error with reference to the source where it occurred.
showError :: Show symbol => Source.Source -> Error symbol -> ShowS
showError source Error{..}
  = showSourcePos errorPos . showString ": error: " . showExpectation . showChar '\n'
  . showString context -- actualLines results include line endings, so no newline here
  . showString (replicate (succ (Info.column errorPos + lineNumberDigits)) ' ') . showChar '^' . showChar '\n'
  where showExpectation = case (errorExpected, errorActual) of
          ([], Nothing) -> showString "no rule to match at end of input nodes"
          (symbols, Nothing) -> showString "expected " . showSymbols symbols . showString " at end of input nodes"
          (symbols, Just a) -> showString "expected " . showSymbols symbols . showString ", but got " . shows a
        context = maybe "\n" (toS . Source.sourceText . sconcat) (nonEmpty [ Source.Source (toS (showLineNumber i)) <> Source.Source ": " <> l | (i, l) <- zip [1..] (Source.actualLines source), inRange (Info.line errorPos - 2, Info.line errorPos) i ])
        showLineNumber n = let s = show n in replicate (lineNumberDigits - length s) ' ' <> s
        lineNumberDigits = succ (floor (logBase 10 (fromIntegral (Info.line errorPos) :: Double)))

showSymbols :: Show symbol => [symbol] -> ShowS
showSymbols [] = showString "end of input nodes"
showSymbols [symbol] = shows symbol
showSymbols [a, b] = shows a . showString " or " . shows b
showSymbols [a, b, c] = shows a . showString ", " . shows b . showString ", or " . shows c
showSymbols (h:t) = shows h . showString ", " . showSymbols t

showSourcePos :: Info.SourcePos -> ShowS
showSourcePos Info.SourcePos{..} = shows line . showChar ':' . shows column

-- | Run an assignment over an AST exhaustively.
assign :: (Symbol grammar, Enum grammar, Eq grammar, Show grammar) => Assignment (Node grammar) a -> Source.Source -> AST grammar -> Result grammar a
assign assignment source = fmap snd . assignAllFrom assignment . makeState source . pure

assignAllFrom :: (Symbol grammar, Enum grammar, Eq grammar, Show grammar) => Assignment (Node grammar) a -> AssignmentState grammar -> Result grammar (AssignmentState grammar, a)
assignAllFrom assignment state = case runAssignment assignment state of
  Result es (Just (state, a)) -> case stateNodes (dropAnonymous state) of
    [] -> Result [] (Just (state, a))
    Rose (s :. _) _ :_ -> Result (if null es then [ Error (statePos state) [] (Just s) ] else es) Nothing
  r -> r

-- | Run an assignment of nodes in a grammar onto terms in a syntax.
runAssignment :: forall grammar a. (Symbol grammar, Enum grammar, Eq grammar, Show grammar) => Assignment (Node grammar) a -> AssignmentState grammar -> Result grammar (AssignmentState grammar, a)
runAssignment = iterFreer run . fmap (\ a state -> Result [] (Just (state, a)))
  where run :: AssignmentF (Node grammar) x -> (x -> AssignmentState grammar -> Result grammar (AssignmentState grammar, a)) -> AssignmentState grammar -> Result grammar (AssignmentState grammar, a)
        run assignment yield initialState = case (assignment, stateNodes) of
          (Location, Rose (_ :. location) _ : _) -> yield location state
          (Location, []) -> yield (Info.Range stateOffset stateOffset :. Info.SourceSpan statePos statePos :. Nil) state
          (Source, Rose (_ :. range :. _) _ : _) -> yield (Source.sourceText (Source.slice (offsetRange range (negate stateOffset)) stateSource)) (advanceState state)
          (Children childAssignment, Rose _ children : _) -> case assignAllFrom childAssignment state { stateNodes = children } of
            Result _ (Just (state', a)) -> yield a (advanceState state' { stateNodes = stateNodes })
            Result es Nothing -> Result es Nothing
          (Choose choices, Rose (symbol :. _) _ : _) | Just a <- IntMap.lookup (fromEnum symbol) choices -> yield a state
          -- Nullability: some rules, e.g. 'pure a' and 'many a', should match at the end of input. Either side of an alternation may be nullable, ergo Alt can match at the end of input.
          (Alt a b, _) -> yield a state <|> yield b state
          (_, []) -> Result [ Error statePos expectedSymbols Nothing ] Nothing
          (_, Rose (symbol :. _ :. nodeSpan :. Nil) _:_) -> Result [ Error (Info.spanStart nodeSpan) expectedSymbols (Just symbol) ] Nothing
          where state@AssignmentState{..} = case assignment of
                  Choose choices | all ((== Regular) . symbolType) (choiceSymbols choices) -> dropAnonymous initialState
                  _ -> initialState
                expectedSymbols = case assignment of
                  Choose choices -> choiceSymbols choices
                  _ -> []
                choiceSymbols choices = ((toEnum :: Int -> grammar) <$> IntMap.keys choices)

dropAnonymous :: Symbol grammar => AssignmentState grammar -> AssignmentState grammar
dropAnonymous state = state { stateNodes = dropWhile ((/= Regular) . symbolType . rhead . roseValue) (stateNodes state) }

-- | Advances the state past the current (head) node (if any), dropping it off stateNodes & its corresponding bytes off of stateSource, and updating stateOffset & statePos to its end. Exhausted 'AssignmentState's (those without any remaining nodes) are returned unchanged.
advanceState :: AssignmentState grammar -> AssignmentState grammar
advanceState state@AssignmentState{..}
  | Rose (_ :. range :. span :. _) _ : rest <- stateNodes = AssignmentState (Info.end range) (Info.spanEnd span) (Source.drop (Info.end range - stateOffset) stateSource) rest
  | otherwise = state

-- | State kept while running 'Assignment's.
data AssignmentState grammar = AssignmentState
  { stateOffset :: Int -- ^ The offset into the Source thus far reached, measured in bytes.
  , statePos :: Info.SourcePos -- ^ The (1-indexed) line/column position in the Source thus far reached.
  , stateSource :: Source.Source -- ^ The remaining Source. Equal to dropping 'stateOffset' bytes off the original input Source.
  , stateNodes :: [AST grammar] -- ^ The remaining nodes to assign. Note that 'children' rules recur into subterms, and thus this does not necessarily reflect all of the terms remaining to be assigned in the overall algorithm, only those “in scope.”
  }
  deriving (Eq, Show)

makeState :: Source.Source -> [AST grammar] -> AssignmentState grammar
makeState source nodes = AssignmentState 0 (Info.SourcePos 1 1) source nodes


-- Instances

instance Enum symbol => Alternative (Assignment (Node symbol)) where
  empty = Empty `Then` return
  a <|> b = case (a, b) of
    (_, Empty `Then` _) -> a
    (Empty `Then` _, _) -> b
    (Choose choices1 `Then` continue1, Choose choices2 `Then` continue2) -> Choose (IntMap.union (fmap continue1 choices1) (fmap continue2 choices2)) `Then` identity
    _ -> wrap $ Alt a b

instance Show symbol => Show1 (AssignmentF (Node symbol)) where
  liftShowsPrec sp sl d a = case a of
    Location -> showString "Location" . sp d (Info.Range 0 0 :. Info.SourceSpan (Info.SourcePos 0 0) (Info.SourcePos 0 0) :. Nil)
    Source -> showString "Source" . showChar ' ' . sp d ""
    Children a -> showsUnaryWith (liftShowsPrec sp sl) "Children" d a
    Choose choices -> showsUnaryWith (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) "Choose" d (IntMap.toList choices)
    Alt a b -> showsBinaryWith sp sp "Alt" d a b
    Empty -> showString "Empty"

type instance Base (Rose a) = RoseF a

data RoseF a f = RoseF a [f]
  deriving (Eq, Foldable, Functor, Show, Traversable)

instance Recursive (Rose a) where project (Rose a as) = RoseF a as
instance Corecursive (Rose a) where embed (RoseF a as) = Rose a as

instance Show2 Result where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (Result es a) = showsBinaryWith (liftShowsPrec (liftShowsPrec sp1 sl1) (liftShowList sp1 sl1)) (liftShowsPrec sp2 sl2) "Result" d es a

instance (Show symbol, Show a) => Show (Result symbol a) where
  showsPrec = showsPrec2

instance Show1 Error where
  liftShowsPrec sp sl d (Error p e a) = showsTernaryWith showsPrec (liftShowsPrec sp sl) (liftShowsPrec sp sl) "Error" d p e a
    where showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
            showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

instance Applicative (Result symbol) where
  pure = Result [] . Just
  Result e1 f <*> Result e2 a = Result (e1 <> e2) (f <*> a)

instance Alternative (Result symbol) where
  empty = Result [] Nothing
  Result e (Just a) <|> _ = Result e (Just a)
  Result e1 Nothing <|> Result e2 b = Result (e1 <> e2) b

{-# LANGUAGE DataKinds, GADTs, InstanceSigs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
-- | Assignment of AST onto some other structure (typically terms).
--
--   Parsing yields an AST represented as a Rose tree labelled with symbols in the language’s grammar and source locations (byte Range and SourceSpan). An Assignment represents a (partial) map from AST nodes onto some other structure; in essence, it’s a parser that operates over trees. (For our purposes, this structure is typically Terms annotated with source locations.) Assignments are able to match based on symbol, sequence, and hierarchy; thus, in @x = y@, both @x@ and @y@ might have the same symbol, @Identifier@, the left can be assigned to a variable declaration, while the right can be assigned to a variable reference.
--
--   Assignments can be any of the following primitive rules:
--
--   1. 'symbol' rules match a node against a specific symbol in the source language’s grammar; they succeed iff a) there is a current node, and b) its symbol is equal to the argument symbol. Matching a 'symbol' rule does not advance past the current node, meaning that you can match a node against a symbol and also e.g. match against the node’s 'children'. This also means that some care must be taken, as repeating a symbol with 'many' or 'some' (see below) will never advance past the current node and could therefore loop forever.
--
--   2. 'location' rules always succeed, and produce the current node’s Location (byte Range and SourceSpan). If there is no current node (i.e. if matching has advanced past the root node or past the last child node when operating within a 'children' rule), the location is instead the end of the most recently matched node, specified as a zero-width Range and SourceSpan. 'location' rules do not advance past the current node, meaning that you can both match a node’s 'location' and other properties.
--
--   3. 'source' rules succeed whenever there is a current node (i.e. matching has not advanced past the root node or the last child node when operating within a 'children' rule), and produce its source as a ByteString. 'source' is intended to match leaf nodes such as e.g. comments. 'source' rules advance past the current node.
--
--   4. 'children' rules apply their argument (an assignment) to the children of the current node, succeeding iff a) there is a current node, b) the argument assignment matches the children, and c) there are no (regular) nodes left over (see below re: tokens), producing the result of matching the argument assignment against the children. 'children' rules can match a node with no child nodes if their argument can successfully match at the end of input.
--
--   5. Via the 'Alternative' instance, 'empty' assignments always fail. This can be used (in combination with the 'Monad' instance) to (for example) fail if a 'source' assignment produces an ill-formatted ByteString. However, see below re: committed choice.
--
--   6. Via the 'Applicative' instance, 'pure' (or via the 'Monad' instance, 'return') assignments always succeed, producing the passed value. They do not advance past the current node. In combination with the 'Alternative' instance, 'pure' can provide default values when optional syntax is not present in the AST.
--
--   Assignments can further be combined in a few different ways:
--
--   1. The 'Functor' instance maps values from the AST (Location, ByteString, etc.) into another structure.
--
--   2. The 'Applicative' instance assigns sequences of (sibling) AST nodes in order, as well as providing 'pure' assignments (see above). Most assignments of a single piece of syntax consist of an 'Applicative' chain of assignments.
--
--   3. The 'Alternative' instance chooses between a set of assignments, as well as providing 'empty' assignments (see above). See below re: committed choice for best practices for efficiency & error reporting when it comes to assigning multiple alternatives. Most high-level assignments (e.g. “declaration” or “statement” assignments) consist of choices among two or more 'Applicative' chains of assignments, mirroring the structure of the parser’s choices. The 'Alternative' instance also enables repetitions via the 'many' (≥ 0 repetitions) and 'some' (≥ 1 repetition) methods. Finally, the 'optional' function uses the 'Alternative' instance to assign a value in 'Maybe', succeeding with 'Nothing' when unmatched.
--
--   4. The 'Monad' instance allows assignments to depend on the results of earlier assignments. In general, most assignments should not be written using the 'Monad' instance; however, some specific situations require it, e.g. assigning 'x += y' to be equivalent to 'x = x + y'.
--
--
--   == Best practices
--
--   Because of their flexibility, the same assignment can often be written in multiple different ways. The following best practices should ensure efficient assignment with clear error messages for ill-formed AST.
--
--   === Committed choice
--
--   Assignments can represent alternatives as either committed or uncommitted choices, both written with '<|>'. “Committed” in this context means that a failure in one of the alternatives will not result in backtracking followed by an attempt of one of the other alternatives; thus, committed choice is more efficient. (By the same token, it enables much better error messages since backtracking erases most of the relevant context.) Committed choices are constructed via the following rules:
--
--   1. 'empty' is dropped from choices:
--   prop> empty <|> a = a -- empty is the left-identity of <|>
--   prop> a <|> empty = a -- empty is the right-identity of <|>
--
--   2. 'symbol' rules construct a committed choice (with only a single alternative).
--
--   3. 'fmap' (and by extension '<$>' and '<$') of a committed choice is a committed choice.
--
--   4. '<*>' (and by extension '*>' and '<*') with a committed choice on the left is a committed choice.
--
--   5. '>>=' (and by extension '>>', '=<<', and '<<') of a committed choice is a committed choice. It may be helpful to think of this and the above rule for '<*>' as “sequences starting with committed choices remain committed choices.”
--
--   6. '<|>' of two committed choices is a committed choice.
--
--   Finally, if a given choice is not a committed choice, it is an uncommitted choice.
--
--   Distilling the above, the rule of thumb is to always start an assignment for a given piece of syntax with either a 'symbol' rule or an 'fmap' over a 'symbol' rule. When assigning multiple pieces of syntax, place any known uncommitted choices at the (rightmost) end of the chain; '<|>' is left-associative, so this guarantees that you’re adding at most one uncommitted choice on top of the ones already present.
--
--   === Matching tokens
--
--   AST symbols are classified by their 'symbolType' as either 'Regular', 'Anonymous', or 'Auxiliary'. 'Auxiliary' never appears in ASTs; 'Regular' is for the symbols of explicitly named productions in the grammar, and 'Anonymous' is for unnamed productions of content such as tokens. Most of the time, assignments are only concerned with the named productions, and thus will be using 'Regular' symbols. Therefore, when matching a committed choice of all-'Regular' symbols, nodes with 'Anonymous' symbols will be skipped. However, in some cases grammars don’t provide a named symbol for e.g. every kind of infix operator, and thus the only way to differentiate between them is by means of a 'symbol' rule for an 'Anonymous' token. In these cases, and before every other kind of assignment, the 'Anonymous' nodes will not be skipped so that matching can succeed.
--
--   Therefore, in addition to the rule of thumb for committed choices (see above), try to match 'Regular' symbols up front, and only match 'Anonymous' ones in the middle of a chain. That will ensure that you don’t have to make redundant effort to explicitly skip 'Anonymous' nodes ahead of multiple alternatives, and can instead rely on them being automatically skipped except when explicitly required.
module Data.Syntax.Assignment
( Assignment
, Location
, location
, symbol
, source
, children
, Rose
, Node
, AST
, Result(..)
, Error(..)
, ErrorCause(..)
, showError
, showExpectation
, assign
, runAssignment
, AssignmentState(..)
, makeState
) where

import Control.Monad.Free.Freer
import Data.ByteString (isSuffixOf)
import Data.Functor.Classes
import qualified Data.IntMap.Lazy as IntMap
import Data.Ix (inRange)
import Data.List.NonEmpty (nonEmpty)
import Data.Record
import Data.String
import GHC.Stack
import qualified Info
import Prologue hiding (Alt, get, Location, state)
import Range (offsetRange)
import qualified Source (Source(..), drop, slice, sourceText, actualLines)
import System.Console.ANSI
import Text.Parser.TreeSitter.Language
import Text.Show hiding (show)

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment node = Freer (AssignmentF node)

data AssignmentF node a where
  Location :: HasCallStack => AssignmentF node Location
  Source :: HasCallStack => AssignmentF symbol ByteString
  Children :: HasCallStack => Assignment symbol a -> AssignmentF symbol a
  Choose :: HasCallStack => IntMap.IntMap a -> AssignmentF node a
  Alt :: HasCallStack => a -> a -> AssignmentF symbol a
  Empty :: HasCallStack => AssignmentF symbol a
  Throw :: HasCallStack => Error symbol -> AssignmentF symbol a
  Catch :: HasCallStack => a -> (Error symbol -> a) -> AssignmentF symbol a

-- | Zero-width production of the current location.
--
--   If assigning at the end of input or at the end of a list of children, the loccation will be returned as an empty Range and SourceSpan at the current offset. Otherwise, it will be the Range and SourceSpan of the current node.
location :: HasCallStack => Assignment grammar Location
location = Location `Then` return

-- | Zero-width match of a node with the given symbol, producing the current node’s location.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. 'many (symbol A *> b)' is fine, but 'many (symbol A)' is not.
symbol :: (Enum symbol, Eq symbol, HasCallStack) => symbol -> Assignment symbol Location
symbol s = withFrozenCallStack $ Choose (IntMap.singleton (fromEnum s) ()) `Then` (const location)

-- | A rule to produce a node’s source as a ByteString.
source :: HasCallStack => Assignment symbol ByteString
source = withFrozenCallStack $ Source `Then` return

-- | Match a node by applying an assignment to its children.
children :: HasCallStack => Assignment symbol a -> Assignment symbol a
children forEach = withFrozenCallStack $ Children forEach `Then` return


-- | A rose tree.
type Rose = Cofree []

-- | A location specified as possibly-empty intervals of bytes and line/column positions.
type Location = Record '[Info.Range, Info.SourceSpan]

-- | The label annotating a node in the AST, specified as the pairing of its symbol and location information.
type Node grammar = Record '[Maybe grammar, Info.Range, Info.SourceSpan]

-- | An abstract syntax tree in some 'grammar', with symbols and location information annotating each node.
type AST grammar = Rose (Node grammar)


-- | The result of assignment, possibly containing an error.
data Result symbol a = Result { resultError :: Maybe (Error symbol), resultValue :: Maybe a }
  deriving (Eq, Foldable, Functor, Traversable)

data Error symbol where
  Error
    :: HasCallStack
    => { errorPos :: Info.SourcePos
       , errorCause :: ErrorCause symbol
       } -> Error symbol

deriving instance Eq symbol => Eq (Error symbol)
deriving instance Show symbol => Show (Error symbol)

data ErrorCause symbol
  = UnexpectedSymbol [symbol] symbol
  | UnexpectedEndOfInput [symbol]
  | ParseError [symbol]
  deriving (Eq, Show)

-- | Pretty-print an Error with reference to the source where it occurred.
showError :: Show symbol => Source.Source -> Error symbol -> String
showError source error@Error{..}
  = withSGRCode [SetConsoleIntensity BoldIntensity] (showSourcePos Nothing errorPos) . showString ": " . withSGRCode [SetColor Foreground Vivid Red] (showString "error") . showString ": " . showExpectation error . showChar '\n'
  . showString (toS context) . (if isSuffixOf "\n" context then identity else showChar '\n')
  . showString (replicate (succ (Info.column errorPos + lineNumberDigits)) ' ') . withSGRCode [SetColor Foreground Vivid Green] (showChar '^') . showChar '\n'
  . showString (prettyCallStack callStack)
  $ ""
  where context = maybe "\n" (Source.sourceText . sconcat) (nonEmpty [ Source.Source (toS (showLineNumber i)) <> Source.Source ": " <> l | (i, l) <- zip [1..] (Source.actualLines source), inRange (Info.line errorPos - 2, Info.line errorPos) i ])
        showLineNumber n = let s = show n in replicate (lineNumberDigits - length s) ' ' <> s
        lineNumberDigits = succ (floor (logBase 10 (fromIntegral (Info.line errorPos) :: Double)))
        showSGRCode = showString . setSGRCode
        withSGRCode code s = showSGRCode code . s . showSGRCode []

showExpectation :: Show symbol => Error symbol -> ShowS
showExpectation Error{..} = case errorCause of
  UnexpectedEndOfInput [] -> showString "no rule to match at end of input nodes"
  UnexpectedEndOfInput symbols -> showString "expected " . showSymbols symbols . showString " at end of input nodes"
  UnexpectedSymbol symbols a -> showString "expected " . showSymbols symbols . showString ", but got " . shows a
  ParseError symbols -> showString "expected " . showSymbols symbols . showString ", but got parse error"

showSymbols :: Show symbol => [symbol] -> ShowS
showSymbols [] = showString "end of input nodes"
showSymbols [symbol] = shows symbol
showSymbols [a, b] = shows a . showString " or " . shows b
showSymbols [a, b, c] = shows a . showString ", " . shows b . showString ", or " . shows c
showSymbols (h:t) = shows h . showString ", " . showSymbols t

showSourcePos :: Maybe FilePath -> Info.SourcePos -> ShowS
showSourcePos path Info.SourcePos{..} = maybe (showParen True (showString "interactive")) showString path . showChar ':' . shows line . showChar ':' . shows column

-- | Run an assignment over an AST exhaustively.
assign :: (Symbol grammar, Enum grammar, Eq grammar, HasCallStack) => Assignment grammar a -> Source.Source -> AST grammar -> Result grammar a
assign assignment source = fmap snd . assignAllFrom assignment . makeState source . pure

assignAllFrom :: (Symbol grammar, Enum grammar, Eq grammar, HasCallStack) => Assignment grammar a -> AssignmentState grammar -> Result grammar (AssignmentState grammar, a)
assignAllFrom assignment state = case runAssignment assignment state of
  Result err (Just (state, a)) -> case stateNodes (dropAnonymous state) of
    [] -> Result Nothing (Just (state, a))
    node : _ -> Result (err <|> Just (Error (statePos state) (maybe (ParseError []) (UnexpectedSymbol []) (rhead (extract node))))) Nothing
  r -> r

-- | Run an assignment of nodes in a grammar onto terms in a syntax.
runAssignment :: forall grammar a. (Symbol grammar, Enum grammar, Eq grammar, HasCallStack) => Assignment grammar a -> AssignmentState grammar -> Result grammar (AssignmentState grammar, a)
runAssignment = iterFreer run . fmap (\ a state -> pure (state, a))
  where run :: AssignmentF grammar x -> (x -> AssignmentState grammar -> Result grammar (AssignmentState grammar, a)) -> AssignmentState grammar -> Result grammar (AssignmentState grammar, a)
        run assignment yield initialState = case (assignment, stateNodes) of
          (Location, node : _) -> yield (rtail (extract node)) state
          (Location, []) -> yield (Info.Range stateOffset stateOffset :. Info.SourceSpan statePos statePos :. Nil) state
          (Source, node : _) -> yield (Source.sourceText (Source.slice (offsetRange (Info.byteRange (extract node)) (negate stateOffset)) stateSource)) (advanceState state)
          (Children childAssignment, node : _) -> case assignAllFrom childAssignment state { stateNodes = unwrap node } of
            Result _ (Just (state', a)) -> yield a (advanceState state' { stateNodes = stateNodes })
            Result err Nothing -> Result err Nothing
          (Choose choices, node : _) | (Just symbol :. _) :< _ <- runCofree node, Just a <- IntMap.lookup (fromEnum symbol) choices -> yield a state
          -- Nullability: some rules, e.g. 'pure a' and 'many a', should match at the end of input. Either side of an alternation may be nullable, ergo Alt can match at the end of input.
          (Alt a b, _) -> yield a state <|> yield b state
          (Throw e, _) -> Result (Just e) Nothing
          (Catch during handler, _) -> case yield during state of
            Result _ (Just (state', a)) -> Result Nothing (Just (state', a))
            Result err Nothing -> maybe (Result Nothing Nothing) (flip yield state . handler) err
          (_, []) -> Result (Just (Error statePos (UnexpectedEndOfInput expectedSymbols))) Nothing
          (_, node:_) -> let Info.SourceSpan startPos _ = Info.sourceSpan (extract node) in Result (Just (maybe (Error startPos (ParseError expectedSymbols)) (Error startPos . UnexpectedSymbol expectedSymbols) (rhead (extract node)))) Nothing
          where state@AssignmentState{..} = case assignment of
                  Choose choices | all ((== Regular) . symbolType) (choiceSymbols choices) -> dropAnonymous initialState
                  _ -> initialState
                expectedSymbols = case assignment of
                  Choose choices -> choiceSymbols choices
                  _ -> []
                choiceSymbols choices = ((toEnum :: Int -> grammar) <$> IntMap.keys choices)

dropAnonymous :: Symbol grammar => AssignmentState grammar -> AssignmentState grammar
dropAnonymous state = state { stateNodes = dropWhile ((`notElem` [Just Regular, Nothing]) . fmap symbolType . rhead . extract) (stateNodes state) }

-- | Advances the state past the current (head) node (if any), dropping it off stateNodes & its corresponding bytes off of stateSource, and updating stateOffset & statePos to its end. Exhausted 'AssignmentState's (those without any remaining nodes) are returned unchanged.
advanceState :: AssignmentState grammar -> AssignmentState grammar
advanceState state@AssignmentState{..}
  | node : rest <- stateNodes, (_ :. range :. span :. _) :< _ <- runCofree node = AssignmentState (Info.end range) (Info.spanEnd span) (Source.drop (Info.end range - stateOffset) stateSource) rest
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

instance Enum symbol => Alternative (Assignment symbol) where
  empty :: HasCallStack => Assignment symbol a
  empty = Empty `Then` return
  (<|>) :: HasCallStack => Assignment symbol a -> Assignment symbol a -> Assignment symbol a
  a <|> b = case (a, b) of
    (_, Empty `Then` _) -> a
    (Empty `Then` _, _) -> b
    (Choose choices1 `Then` continue1, Choose choices2 `Then` continue2) -> Choose (IntMap.union (fmap continue1 choices1) (fmap continue2 choices2)) `Then` identity
    _ -> wrap $ Alt a b

instance Show symbol => Show1 (AssignmentF symbol) where
  liftShowsPrec sp sl d a = case a of
    Location -> showString "Location" . sp d (Info.Range 0 0 :. Info.SourceSpan (Info.SourcePos 0 0) (Info.SourcePos 0 0) :. Nil)
    Source -> showString "Source" . showChar ' ' . sp d ""
    Children a -> showsUnaryWith (liftShowsPrec sp sl) "Children" d a
    Choose choices -> showsUnaryWith (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) "Choose" d (IntMap.toList choices)
    Alt a b -> showsBinaryWith sp sp "Alt" d a b
    Empty -> showString "Empty"
    Throw e -> showsUnaryWith showsPrec "Throw" d e
    Catch during handler -> showsBinaryWith sp (const (const (showChar '_'))) "Catch" d during handler

instance Show2 Result where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (Result es a) = showsBinaryWith (liftShowsPrec (liftShowsPrec sp1 sl1) (liftShowList sp1 sl1)) (liftShowsPrec sp2 sl2) "Result" d es a

instance (Show symbol, Show a) => Show (Result symbol a) where
  showsPrec = showsPrec2

instance Show1 Error where
  liftShowsPrec sp sl d (Error p c) = showsBinaryWith showsPrec (liftShowsPrec sp sl) "Error" d p c

instance Show1 ErrorCause where
  liftShowsPrec sp sl d e = case e of
    UnexpectedSymbol expected actual -> showsBinaryWith (liftShowsPrec sp sl) sp "UnexpectedSymbol" d expected actual
    UnexpectedEndOfInput expected -> showsUnaryWith (liftShowsPrec sp sl) "UnexpectedEndOfInput" d expected
    ParseError expected -> showsUnaryWith (liftShowsPrec sp sl) "ParseError" d expected

instance Applicative (Result symbol) where
  pure = Result Nothing . Just
  Result e1 f <*> Result e2 a = Result (e1 <|> e2) (f <*> a)

instance Alternative (Result symbol) where
  empty = Result Nothing Nothing
  Result e (Just a) <|> _ = Result e (Just a)
  Result e1 Nothing <|> Result e2 b = Result (e1 <|> e2) b

instance MonadError (Error symbol) (Assignment symbol) where
  throwError :: HasCallStack => Error symbol -> Assignment symbol a
  throwError error = withFrozenCallStack $ Throw error `Then` return

  catchError :: HasCallStack => Assignment symbol a -> (Error symbol -> Assignment symbol a) -> Assignment symbol a
  catchError during handler = withFrozenCallStack $ Catch during handler `Then` identity

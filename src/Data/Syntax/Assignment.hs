{-# LANGUAGE DataKinds, GADTs, InstanceSigs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators #-}
-- | Assignment of AST onto some other structure (typically terms).
--
--   Parsing yields an AST represented as a Rose tree labelled with symbols in the language’s grammar and source locations (byte Range and Span). An Assignment represents a (partial) map from AST nodes onto some other structure; in essence, it’s a parser that operates over trees. (For our purposes, this structure is typically Terms annotated with source locations.) Assignments are able to match based on symbol, sequence, and hierarchy; thus, in @x = y@, both @x@ and @y@ might have the same symbol, @Identifier@, the left can be assigned to a variable declaration, while the right can be assigned to a variable reference.
--
--   Assignments can be any of the following primitive rules:
--
--   1. 'symbol' rules match a node against a specific symbol in the source language’s grammar; they succeed iff a) there is a current node, and b) its symbol is equal to the argument symbol. Matching a 'symbol' rule does not advance past the current node, meaning that you can match a node against a symbol and also e.g. match against the node’s 'children'. This also means that some care must be taken, as repeating a symbol with 'many' or 'some' (see below) will never advance past the current node and could therefore loop forever.
--
--   2. 'location' rules always succeed, and produce the current node’s Location (byte Range and Span). If there is no current node (i.e. if matching has advanced past the root node or past the last child node when operating within a 'children' rule), the location is instead the end of the most recently matched node, specified as a zero-width Range and Span. 'location' rules do not advance past the current node, meaning that you can both match a node’s 'location' and other properties.
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
-- Types
( Assignment
, Location
, AST
, Node(..)
, nodeLocation
-- Combinators
, location
, Data.Syntax.Assignment.project
, symbol
, source
, children
, while
-- Results
, Error(..)
, ErrorCause(..)
, printError
, withSGRCode
-- Running
, assignBy
, runAssignment
-- Implementation details (for testing)
, AssignmentState(..)
, makeState
) where

import Control.Monad.Free.Freer
import Data.Blob
import Data.ByteString (isSuffixOf)
import Data.Functor.Classes
import Data.Functor.Foldable as F hiding (Nil)
import qualified Data.IntMap.Lazy as IntMap
import Data.Ix (inRange)
import Data.List.NonEmpty (nonEmpty)
import Data.Record
import qualified Data.Source as Source (Source, fromBytes, slice, sourceBytes, sourceLines)
import GHC.Stack
import qualified Info
import Prologue hiding (Alt, get, Location, state)
import System.Console.ANSI
import Text.Parser.TreeSitter.Language
import Text.Show hiding (show)
import System.IO (hIsTerminalDevice, hPutStr)

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment ast grammar = Freer (AssignmentF ast grammar)

data AssignmentF ast grammar a where
  Location :: HasCallStack => AssignmentF ast grammar (Record Location)
  Project :: HasCallStack => (forall x. Base ast x -> a) -> AssignmentF ast grammar a
  Source :: HasCallStack => AssignmentF ast grammar ByteString
  Children :: HasCallStack => Assignment ast grammar a -> AssignmentF ast grammar a
  Choose :: HasCallStack => IntMap.IntMap a -> AssignmentF ast grammar a
  Many :: HasCallStack => Assignment ast grammar a -> AssignmentF ast grammar [a]
  Alt :: HasCallStack => a -> a -> AssignmentF ast grammar a
  Throw :: HasCallStack => Error grammar -> AssignmentF ast grammar a
  Catch :: HasCallStack => a -> (Error grammar -> a) -> AssignmentF ast grammar a

-- | Zero-width production of the current location.
--
--   If assigning at the end of input or at the end of a list of children, the loccation will be returned as an empty Range and Span at the current offset. Otherwise, it will be the Range and Span of the current node.
location :: HasCallStack => Assignment ast grammar (Record Location)
location = Location `Then` return

-- | Zero-width projection of the current node.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. @many (project f *> b)@ is fine, but @many (project f)@ is not.
project :: HasCallStack => (forall x. Base ast x -> a) -> Assignment ast grammar a
project projection = Project projection `Then` return

-- | Zero-width match of a node with the given symbol, producing the current node’s location.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. @many (symbol A *> b)@ is fine, but @many (symbol A)@ is not.
symbol :: (Enum grammar, Eq grammar, HasCallStack) => grammar -> Assignment ast grammar (Record Location)
symbol s = withFrozenCallStack $ Choose (IntMap.singleton (fromEnum s) ()) `Then` (const location)

-- | A rule to produce a node’s source as a ByteString.
source :: HasCallStack => Assignment ast grammar ByteString
source = withFrozenCallStack $ Source `Then` return

-- | Match a node by applying an assignment to its children.
children :: HasCallStack => Assignment ast grammar a -> Assignment ast grammar a
children forEach = withFrozenCallStack $ Children forEach `Then` return

-- | Collect a list of values until one fails a predicate.
while :: (Alternative m, Monad m) => (a -> Bool) -> m a -> m [a]
while predicate step = many $ do
  result <- step
  guard (predicate result)
  pure result


-- | A location specified as possibly-empty intervals of bytes and line/column positions.
type Location = '[Info.Range, Info.Span]

-- | An AST node labelled with symbols and source location.
type AST grammar = Cofree [] (Node grammar)

data Node grammar = Node
  { nodeSymbol :: !grammar
  , nodeByteRange :: !Info.Range
  , nodeSpan :: !Info.Span
  }
  deriving (Eq, Show)

nodeLocation :: Node grammar -> Record Location
nodeLocation Node{..} = nodeByteRange :. nodeSpan :. Nil


data Error grammar where
  Error
    :: HasCallStack
    => { errorPos :: Info.Pos
       , errorCause :: ErrorCause grammar
       } -> Error grammar

deriving instance Eq grammar => Eq (Error grammar)
deriving instance Show grammar => Show (Error grammar)

data ErrorCause grammar
  = UnexpectedSymbol [grammar] grammar
  | UnexpectedEndOfInput [grammar]
  deriving (Eq, Show)

-- | Pretty-print an Error with reference to the source where it occurred.
printError :: Show grammar => Blob -> Error grammar -> IO ()
printError Blob{..} error@Error{..} = do
  withSGRCode [SetConsoleIntensity BoldIntensity] . putStrErr $ showPos (maybe Nothing (const (Just blobPath)) blobKind) errorPos . showString ": "
  withSGRCode [SetColor Foreground Vivid Red] . putStrErr $ showString "error" . showString ": " . showExpectation error . showChar '\n'
  putStrErr $ showString (toS context) . (if isSuffixOf "\n" context then identity else showChar '\n') . showString (replicate (succ (Info.posColumn errorPos + lineNumberDigits)) ' ')
  withSGRCode [SetColor Foreground Vivid Green] . putStrErr $ showChar '^' . showChar '\n'
  putStrErr $ showString (prettyCallStack callStack) . showChar '\n'
  where context = maybe "\n" (Source.sourceBytes . sconcat) (nonEmpty [ Source.fromBytes (toS (showLineNumber i)) <> Source.fromBytes ": " <> l | (i, l) <- zip [1..] (Source.sourceLines blobSource), inRange (Info.posLine errorPos - 2, Info.posLine errorPos) i ])
        showLineNumber n = let s = show n in replicate (lineNumberDigits - length s) ' ' <> s
        lineNumberDigits = succ (floor (logBase 10 (fromIntegral (Info.posLine errorPos) :: Double)))
        putStrErr = hPutStr stderr . ($ "")

withSGRCode :: [SGR] -> IO a -> IO ()
withSGRCode code action = do
  isTerm <- hIsTerminalDevice stderr
  if isTerm then do
    _ <- hSetSGR stderr code
    _ <- action
    hSetSGR stderr []
  else do
    _ <- action
    pure ()

showExpectation :: Show grammar => Error grammar -> ShowS
showExpectation Error{..} = case errorCause of
  UnexpectedEndOfInput [] -> showString "no rule to match at end of input nodes"
  UnexpectedEndOfInput symbols -> showString "expected " . showSymbols symbols . showString " at end of input nodes"
  UnexpectedSymbol symbols a -> showString "expected " . showSymbols symbols . showString ", but got " . shows a

showSymbols :: Show grammar => [grammar] -> ShowS
showSymbols [] = showString "end of input nodes"
showSymbols [symbol] = shows symbol
showSymbols [a, b] = shows a . showString " or " . shows b
showSymbols [a, b, c] = shows a . showString ", " . shows b . showString ", or " . shows c
showSymbols (h:t) = shows h . showString ", " . showSymbols t

showPos :: Maybe FilePath -> Info.Pos -> ShowS
showPos path Info.Pos{..} = maybe (showParen True (showString "interactive")) showString path . showChar ':' . shows posLine . showChar ':' . shows posColumn

-- | Run an assignment over an AST exhaustively.
assignBy :: (Symbol grammar, Enum grammar, Eq grammar, Recursive ast, Foldable (Base ast), HasCallStack)
         => (forall x. Base ast x -> Node grammar) -- ^ A function to project a 'Node' from the ast.
         -> Source.Source                          -- ^ The source for the parse tree.
         -> Assignment ast grammar a               -- ^ The 'Assignment to run.
         -> ast                                    -- ^ The root of the ast.
         -> Either (Error grammar) a               -- ^ 'Either' an 'Error' or the assigned value.
assignBy toNode source assignment = fmap fst . runAssignment toNode source assignment . makeState . pure

-- | Run an assignment of nodes in a grammar onto terms in a syntax over an AST exhaustively.
runAssignment :: forall grammar a ast. (Symbol grammar, Enum grammar, Eq grammar, Recursive ast, Foldable (Base ast), HasCallStack)
              => (forall x. Base ast x -> Node grammar)                  -- ^ A function to project a 'Node' from the ast.
              -> Source.Source                                           -- ^ The source for the parse tree.
              -> Assignment ast grammar a                                -- ^ The 'Assignment' to run.
              -> AssignmentState ast grammar                             -- ^ The current state.
              -> Either (Error grammar) (a, AssignmentState ast grammar) -- ^ 'Either' an 'Error' or the pair of the assigned value & updated state.
runAssignment toNode source assignment state = go assignment state >>= requireExhaustive
  where go :: Assignment ast grammar result -> AssignmentState ast grammar -> Either (Error grammar) (result, AssignmentState ast grammar)
        go = iterFreer run . fmap ((pure .) . (,))
        {-# INLINE go #-}

        run :: AssignmentF ast grammar x
            -> (x -> AssignmentState ast grammar -> Either (Error grammar) (result, AssignmentState ast grammar))
            -> AssignmentState ast grammar
            -> Either (Error grammar) (result, AssignmentState ast grammar)
        run assignment yield initialState = case assignment of
          Location -> yield location state
          Project projection | Just node <- headNode -> yield (projection (F.project node)) state
          Source | Just node <- headNode -> yield (Source.sourceBytes (Source.slice (nodeByteRange (projectNode node)) source)) (advanceState state)
          Children childAssignment | Just node <- headNode -> do
            (a, state') <- go childAssignment state { stateNodes = toList (F.project node) } >>= requireExhaustive
            yield a (advanceState state' { stateNodes = stateNodes state })
          Choose choices | Just choice <- flip IntMap.lookup choices . fromEnum . nodeSymbol . projectNode =<< headNode -> yield choice state
          Many rule -> uncurry yield (runMany rule state)
          Alt a b -> either (yield b . setStateError state . Just) Right (yield a state)
          Throw e -> Left e
          Catch during handler -> either (flip yield state . handler) Right (yield during state)
          _ | Just (Node symbol _ (Info.Span spanStart _)) <- projectNode <$> headNode -> Left (Error spanStart (UnexpectedSymbol expectedSymbols symbol))
            | otherwise -> Left (Error (statePos state) (UnexpectedEndOfInput expectedSymbols))
          where state | any ((/= Regular) . symbolType) expectedSymbols = dropAnonymous initialState
                      | otherwise = initialState
                expectedSymbols | Choose choices <- assignment = choiceSymbols choices
                                | otherwise = []
                choiceSymbols choices = (toEnum :: Int -> grammar) <$> IntMap.keys choices
                headNode = listToMaybe (stateNodes state)
                location = maybe (Info.Range (stateOffset state) (stateOffset state) :. Info.Span (statePos state) (statePos state) :. Nil) (nodeLocation . projectNode) headNode
        {-# INLINE run #-}

        runMany :: Assignment ast grammar result -> AssignmentState ast grammar -> ([result], AssignmentState ast grammar)
        runMany rule state = case go rule state of
          Left err -> ([], state { stateError = Just err })
          Right (a, state') | ((/=) `on` stateCounter) state state' ->
                                let (as, state'') = runMany rule state'
                                in as `seq` (a : as, state'')
                            | otherwise -> ([a], state')
        {-# INLINE runMany #-}

        requireExhaustive :: (result, AssignmentState ast grammar) -> Either (Error grammar) (result, AssignmentState ast grammar)
        requireExhaustive (a, state) = case stateNodes (dropAnonymous state) of
          [] -> Right (a, state)
          node : _ | Node nodeSymbol _ (Info.Span spanStart _) <- projectNode node ->
            Left $ fromMaybe (Error spanStart (UnexpectedSymbol [] nodeSymbol)) (stateError state)

        dropAnonymous state = state { stateNodes = dropWhile ((/= Regular) . symbolType . nodeSymbol . projectNode) (stateNodes state) }

        -- Advances the state past the current (head) node (if any), dropping it off stateNodes, and updating stateOffset & statePos to its end; or else returns the state unchanged.
        advanceState state@AssignmentState{..}
          | node : rest <- stateNodes
          , Node{..} <- projectNode node = AssignmentState (Info.end nodeByteRange) (Info.spanEnd nodeSpan) stateError (succ stateCounter) rest
          | otherwise = state

        projectNode = toNode . F.project

-- | State kept while running 'Assignment's.
data AssignmentState ast grammar = AssignmentState
  { stateOffset :: Int                  -- ^ The offset into the Source thus far reached, measured in bytes.
  , statePos :: Info.Pos                -- ^ The (1-indexed) line/column position in the Source thus far reached.
  , stateError :: Maybe (Error grammar) -- ^ The most recently encountered error. Preserved for improved error messages in the presence of backtracking.
  , stateCounter :: Int                 -- ^ Always incrementing counter that tracks how many nodes have been visited.
  , stateNodes :: [ast]                 -- ^ The remaining nodes to assign. Note that 'children' rules recur into subterms, and thus this does not necessarily reflect all of the terms remaining to be assigned in the overall algorithm, only those “in scope.”
  }
  deriving (Eq, Show)

makeState :: [ast] -> AssignmentState ast grammar
makeState = AssignmentState 0 (Info.Pos 1 1) Nothing 0

setStateError :: AssignmentState ast grammar -> Maybe (Error grammar) -> AssignmentState ast grammar
setStateError state error = state { stateError = error }


-- Instances

instance Enum grammar => Alternative (Assignment ast grammar) where
  empty :: HasCallStack => Assignment ast grammar a
  empty = Choose mempty `Then` return
  (<|>) :: HasCallStack => Assignment ast grammar a -> Assignment ast grammar a -> Assignment ast grammar a
  Return a <|> _ = Return a
  a        <|> b | Just c <- (liftA2 (<>) `on` choices) a b = Choose c `Then` identity
                 | otherwise = wrap $ Alt a b
    where choices :: Assignment ast grammar a -> Maybe (IntMap (Assignment ast grammar a))
          choices (Choose choices `Then` continue) = Just (continue <$> choices)
          choices (Many rule `Then` continue) = fmap (const (Many rule `Then` continue)) <$> choices rule
          choices _ = Nothing
  many :: HasCallStack => Assignment ast grammar a -> Assignment ast grammar [a]
  many a = Many a `Then` return

instance Show grammar => Show1 (AssignmentF ast grammar) where
  liftShowsPrec sp sl d a = case a of
    Location -> showString "Location" . sp d (Info.Range 0 0 :. Info.Span (Info.Pos 1 1) (Info.Pos 1 1) :. Nil)
    Project projection -> showsUnaryWith (const (const (showChar '_'))) "Project" d projection
    Source -> showString "Source" . showChar ' ' . sp d ""
    Children a -> showsUnaryWith (liftShowsPrec sp sl) "Children" d a
    Choose choices -> showsUnaryWith (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) "Choose" d (IntMap.toList choices)
    Many a -> showsUnaryWith (liftShowsPrec (\ d a -> sp d [a]) (sl . pure)) "Many" d a
    Alt a b -> showsBinaryWith sp sp "Alt" d a b
    Throw e -> showsUnaryWith showsPrec "Throw" d e
    Catch during handler -> showsBinaryWith sp (const (const (showChar '_'))) "Catch" d during handler

instance Show1 Error where
  liftShowsPrec sp sl d (Error p c) = showsBinaryWith showsPrec (liftShowsPrec sp sl) "Error" d p c

instance Show1 ErrorCause where
  liftShowsPrec sp sl d e = case e of
    UnexpectedSymbol expected actual -> showsBinaryWith (liftShowsPrec sp sl) sp "UnexpectedSymbol" d expected actual
    UnexpectedEndOfInput expected -> showsUnaryWith (liftShowsPrec sp sl) "UnexpectedEndOfInput" d expected

instance MonadError (Error grammar) (Assignment ast grammar) where
  throwError :: HasCallStack => Error grammar -> Assignment ast grammar a
  throwError error = withFrozenCallStack $ Throw error `Then` return

  catchError :: HasCallStack => Assignment ast grammar a -> (Error grammar -> Assignment ast grammar a) -> Assignment ast grammar a
  catchError during handler = withFrozenCallStack $ Catch during handler `Then` identity

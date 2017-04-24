{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
module Data.Syntax.Assignment
( Assignment
, get
, state
, Location
, location
, symbol
, range
, sourceSpan
, source
, children
, Rose(..)
, RoseF(..)
, Node
, AST
, Result(..)
, assignAll
, runAssignment
, AssignmentState(..)
) where

import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Nil)
import Data.Record
import Data.Text (unpack)
import qualified Info
import Prologue hiding (Alt, get, Location, state)
import Range (offsetRange)
import qualified Source (Source(..), drop, slice, sourceText)
import Text.Parser.TreeSitter.Language
import Text.Show hiding (show)

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment node = Freer (AssignmentF node)

data AssignmentF node a where
  Get :: AssignmentF node node
  State :: AssignmentF (Node grammar) (AssignmentState grammar)
  Source :: AssignmentF symbol ByteString
  Children :: Assignment symbol a -> AssignmentF symbol a
  Alt :: a -> a -> AssignmentF symbol a
  Empty :: AssignmentF symbol a

-- | Zero-width production of the current node.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. 'many (get *> b)' is fine, but 'many get' is not.
get :: Assignment (Record fields) (Record fields)
get = Get `Then` return

-- | Zero-width production of the current state.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. 'many (state *> b)' is fine, but 'many state' is not.
state :: Assignment (Node grammar) (AssignmentState grammar)
state = State `Then` return

-- | Zero-width production of the current location.
--
--   If assigning at the end of input or at the end of a list of children, the loccation will be returned as an empty Range and SourceSpan at the current offset. Otherwise, it will be the Range and SourceSpan of the current node.
location :: Assignment (Node grammar) Location
location = rtail <$> get <|> (\ (AssignmentState o p _ _) -> Info.Range o o :. Info.SourceSpan p p :. Nil) <$> state

-- | Zero-width match of a node with the given symbol.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. 'many (symbol A *> b)' is fine, but 'many (symbol A)' is not.
symbol :: (HasField fields symbol, Eq symbol) => symbol -> Assignment (Record fields) ()
symbol s = Get `Then` guard . (s ==) . getField

-- | Zero-width production of the current node’s range.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. 'many (range *> b)' is fine, but 'many range' is not.
range :: HasField fields Info.Range => Assignment (Record fields) Info.Range
range = Get `Then` return . getField

-- | Zero-width production of the current node’s sourceSpan.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. 'many (sourceSpan *> b)' is fine, but 'many sourceSpan' is not.
sourceSpan :: HasField fields Info.SourceSpan => Assignment (Record fields) Info.SourceSpan
sourceSpan = Get `Then` return . getField

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
data Result a = Result a | Error [Text]
  deriving (Eq, Foldable, Functor, Traversable)


-- | Run an assignment of nodes in a grammar onto terms in a syntax, discarding any unparsed nodes.
assignAll :: (Symbol grammar, Eq grammar, Show grammar) => Assignment (Node grammar) a -> Source.Source -> [AST grammar] -> Result a
assignAll assignment = (assignAllFrom assignment .) . AssignmentState 0 (Info.SourcePos 1 1)

assignAllFrom :: (Symbol grammar, Eq grammar, Show grammar) => Assignment (Node grammar) a -> AssignmentState grammar -> Result a
assignAllFrom assignment state = case runAssignment assignment state of
  Result (state, a) -> case stateNodes (dropAnonymous state) of
    [] -> Result a
    c:_ -> Error ["Expected end of input, but got: " <> show c]
  Error e -> Error e

-- | Run an assignment of nodes in a grammar onto terms in a syntax.
runAssignment :: (Symbol grammar, Eq grammar, Show grammar) => Assignment (Node grammar) a -> AssignmentState grammar -> Result (AssignmentState grammar, a)
runAssignment = iterFreer (\ assignment yield state -> case (assignment, dropAnonymous state) of
  -- Nullability: some rules, e.g. 'pure a' and 'many a', should match at the end of input. Either side of an alternation may be nullable, ergo Alt can match at the end of input.
  (Alt a b, state) -> yield a state <|> yield b state -- FIXME: Symbol `Alt` Symbol `Alt` Symbol is inefficient, should build and match against an IntMap instead.
  (State, state) -> yield state state
  (assignment, AssignmentState offset _ source (subtree@(Rose node@(_ :. range :. _) children) : _)) -> case assignment of
    Get -> yield node state
    Source -> yield (Source.sourceText (Source.slice (offsetRange range (negate offset)) source)) (advanceState state)
    Children childAssignment -> do
      c <- assignAllFrom childAssignment state { stateNodes = children }
      yield c (advanceState state)
    _ -> Error ["No rule to match " <> show subtree]
  (Get, AssignmentState{}) -> Error [ "Expected node but got end of input." ]
  (Source, AssignmentState{}) -> Error [ "Expected leaf node but got end of input." ]
  (Children _, AssignmentState{}) -> Error [ "Expected branch node but got end of input." ]
  _ -> Error ["No rule to match at end of input."])
  . fmap (\ a state -> Result (state, a))

dropAnonymous :: Symbol grammar => AssignmentState grammar -> AssignmentState grammar
dropAnonymous state = state { stateNodes = dropWhile ((/= Regular) . symbolType . rhead . roseValue) (stateNodes state) }

advanceState :: AssignmentState grammar -> AssignmentState grammar
advanceState state@AssignmentState{..}
  | Rose (_ :. range :. span :. _) _ : rest <- stateNodes = AssignmentState (Info.end range) (Info.spanEnd span) (Source.drop (Info.end range - stateOffset) stateSource) rest
  | otherwise = state

data AssignmentState grammar = AssignmentState
  { stateOffset :: Int -- ^ The offset into the Source thus far reached, measured in bytes.
  , statePos :: Info.SourcePos -- ^ The (1-indexed) line/column position in the Source thus far reached.
  , stateSource :: Source.Source -- ^ The remaining Source. Equal to dropping 'stateOffset' bytes off the original input Source.
  , stateNodes :: [AST grammar] -- ^ The remaining nodes to assign. Note that 'children' rules recur into subterms, and thus this does not necessarily reflect all of the terms remaining to be assigned in the overall algorithm, only those “in scope.”
  }
  deriving (Eq, Show)

instance Alternative (Assignment symbol) where
  empty = Empty `Then` return
  (<|>) = (wrap .) . Alt

instance Show symbol => Show1 (AssignmentF symbol) where
  liftShowsPrec sp sl d a = case a of
    Get -> showString "Get"
    State -> showString "State" . sp d (AssignmentState 0 (Info.SourcePos 0 0) (Source.Source "") [])
    Source -> showString "Source" . showChar ' ' . sp d ""
    Children a -> showsUnaryWith (liftShowsPrec sp sl) "Children" d a
    Alt a b -> showsBinaryWith sp sp "Alt" d a b
    Empty -> showString "Empty"

type instance Base (Rose a) = RoseF a

data RoseF a f = RoseF a [f]
  deriving (Eq, Foldable, Functor, Show, Traversable)

instance Recursive (Rose a) where project (Rose a as) = RoseF a as
instance Corecursive (Rose a) where embed (RoseF a as) = Rose a as

instance Show1 Result where
  liftShowsPrec _ _ d (Error es) = showsUnaryWith (const (foldr ((.) . (showString . unpack)) identity)) "Error" d es
  liftShowsPrec sp _ d (Result a) = showsUnaryWith sp "Result" d a

instance Show a => Show (Result a) where
  showsPrec = showsPrec1

instance Applicative Result where
  pure = Result
  Error a <*> Error b = Error (a <> b)
  Error a <*> _ = Error a
  _ <*> Error b = Error b
  Result f <*> Result a = Result (f a)

instance Alternative Result where
  empty = Error []
  Result a <|> _ = Result a
  _ <|> Result b = Result b
  Error a <|> Error b = Error (a <> b)

instance Monad Result where
  return = pure
  Error a >>= _ = Error a
  Result a >>= f = f a

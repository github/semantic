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
, assignAll
, runAssignment
, AssignmentState(..)
) where

import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Nil)
import qualified Data.IntMap.Lazy as IntMap
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
data Result a = Result a | Error [Text]
  deriving (Eq, Foldable, Functor, Traversable)


-- | Run an assignment of nodes in a grammar onto terms in a syntax, discarding any unparsed nodes.
assignAll :: (Symbol grammar, Enum grammar, Eq grammar, Show grammar) => Assignment (Node grammar) a -> Source.Source -> [AST grammar] -> Result a
assignAll assignment = (assignAllFrom assignment .) . AssignmentState 0 (Info.SourcePos 1 1)

assignAllFrom :: (Symbol grammar, Enum grammar, Eq grammar, Show grammar) => Assignment (Node grammar) a -> AssignmentState grammar -> Result a
assignAllFrom assignment state = case runAssignment assignment state of
  Result (state, a) -> case stateNodes (dropAnonymous state) of
    [] -> Result a
    c:_ -> Error ["Expected end of input, but got: " <> show c]
  Error e -> Error e

-- | Run an assignment of nodes in a grammar onto terms in a syntax.
runAssignment :: forall grammar a. (Symbol grammar, Enum grammar, Eq grammar, Show grammar) => Assignment (Node grammar) a -> AssignmentState grammar -> Result (AssignmentState grammar, a)
runAssignment = iterFreer run . fmap (\ a state -> Result (state, a))
  where run :: AssignmentF (Node grammar) x -> (x -> AssignmentState grammar -> Result (AssignmentState grammar, a)) -> AssignmentState grammar -> Result (AssignmentState grammar, a)
        run assignment yield initialState = case (assignment, state) of
          -- Nullability: some rules, e.g. 'pure a' and 'many a', should match at the end of input. Either side of an alternation may be nullable, ergo Alt can match at the end of input.
          (Alt a b, state) -> yield a state <|> yield b state
          (assignment, state@(AssignmentState offset _ source (subtree@(Rose (symbol :. range :. span :. Nil) children) : _))) -> case assignment of
            Location -> yield (range :. span :. Nil) state
            Source -> yield (Source.sourceText (Source.slice (offsetRange range (negate offset)) source)) (advanceState state)
            Children childAssignment -> do
              c <- assignAllFrom childAssignment state { stateNodes = children }
              yield c (advanceState state)
            Choose choices -> case IntMap.lookup (fromEnum symbol) choices of
              Just a -> yield a state
              Nothing -> Error ["Expected one of " <> showChoices choices <> " but got " <> show symbol]
            _ -> Error ["No rule to match " <> show subtree]
          (Location, state@AssignmentState{..}) -> yield (Info.Range stateOffset stateOffset :. Info.SourceSpan statePos statePos :. Nil) state
          (Source, AssignmentState{}) -> Error [ "Expected leaf node but got end of input." ]
          (Children _, AssignmentState{}) -> Error [ "Expected branch node but got end of input." ]
          (Choose choices, AssignmentState{}) -> Error [ "Expected one of " <> showChoices choices <> " but got end of input." ]
          _ -> Error ["No rule to match at end of input."]
          where state@AssignmentState{..} = dropAnonymous initialState

        showChoices :: IntMap.IntMap b -> Text
        showChoices = show . fmap (toEnum :: Int -> grammar) . IntMap.keys

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
    Choose choices -> showsUnaryWith (liftShowsPrec sp sl) "Choose" d choices
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

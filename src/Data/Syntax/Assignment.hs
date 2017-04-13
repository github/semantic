{-# LANGUAGE GADTs, TypeFamilies #-}
module Data.Syntax.Assignment
( Assignment
, rule
, content
, children
, Rose(..)
, RoseF(..)
, Node(..)
, AST
, assignAll
, runAssignment
) where

import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.Functor.Foldable
import Prologue hiding (Alt)
import Text.Show

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment symbol = Freer (AssignmentF symbol)

data AssignmentF symbol a where
  Rule :: symbol -> AssignmentF symbol ()
  Content :: AssignmentF symbol ByteString
  Children :: Assignment symbol a -> AssignmentF symbol a
  Alt :: a -> a -> AssignmentF symbol a
  Empty :: AssignmentF symbol a

-- | Zero-width match of a node with the given symbol.
--
--   Since this is zero-width, care must be taken not to repeat it without chaining on other rules. I.e. 'many (rule A *> b)' is fine, but 'many (rule A)' is not.
rule :: symbol -> Assignment symbol ()
rule symbol = Rule symbol `Then` return

-- | A rule to produce a node’s content as a ByteString.
content :: Assignment symbol ByteString
content = Content `Then` return

-- | Match a node by applying an assignment to its children.
children :: Assignment symbol a -> Assignment symbol a
children forEach = Children forEach `Then` return


-- | A rose tree.
data Rose a = Rose a [Rose a]
  deriving (Eq, Functor, Show)

-- | A node in the input AST. We only concern ourselves with its symbol (considered as an element of 'grammar') and content.
data Node grammar = Node { nodeSymbol :: grammar, nodeContent :: ByteString }
  deriving (Eq, Show)

-- | An abstract syntax tree.
type AST grammar = Rose (Node grammar)


-- | The result of assignment, possibly containing an error.
data Result a = Result a | Error [Text]
  deriving (Eq, Foldable, Functor, Show, Traversable)


-- | Run an assignment of nodes in a grammar onto terms in a syntax, discarding any unparsed nodes.
assignAll :: Eq grammar => Assignment grammar a -> [AST grammar] -> Maybe a
assignAll assignment nodes = case runAssignment assignment nodes of
  Just ([], a) -> Just a
  _ -> Nothing

-- | Run an assignment of nodes in a grammar onto terms in a syntax.
runAssignment :: Eq grammar => Assignment grammar a -> [AST grammar] -> Maybe ([AST grammar], a)
runAssignment = iterFreer (\ assignment yield nodes -> case (assignment, nodes) of
  -- Nullability: some rules, e.g. 'pure a' and 'many a', should match at the end of input. Either side of an alternation may be nullable, ergo Alt can match at the end of input.
  (Alt a b, nodes) -> yield a nodes <|> yield b nodes -- FIXME: Rule `Alt` Rule `Alt` Rule is inefficient, should build and match against an IntMap instead.
  (assignment, Rose Node{..} children : rest) -> case assignment of
    Rule symbol -> guard (symbol == nodeSymbol) >> yield () nodes
    Content -> yield nodeContent rest
    Children childAssignment -> assignAll childAssignment children >>= flip yield rest
    _ -> Nothing
  _ -> Nothing)
  . fmap ((Just .) . flip (,))


instance Alternative (Assignment symbol) where
  empty = Empty `Then` return
  (<|>) = (wrap .) . Alt

instance Show symbol => Show1 (AssignmentF symbol) where
  liftShowsPrec sp sl d a = case a of
    Rule s -> showsUnaryWith showsPrec "Rule" d s . showChar ' ' . sp d ()
    Content -> showString "Content" . showChar ' ' . sp d ""
    Children a -> showsUnaryWith (liftShowsPrec sp sl) "Children" d a
    Alt a b -> showsBinaryWith sp sp "Alt" d a b
    Empty -> showString "Empty"

type instance Base (Rose a) = RoseF a

data RoseF a f = RoseF a [f]
  deriving (Eq, Foldable, Functor, Show, Traversable)

instance Recursive (Rose a) where project (Rose a as) = RoseF a as
instance Corecursive (Rose a) where embed (RoseF a as) = Rose a as

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

{-# LANGUAGE GADTs #-}
module Data.Syntax.Assignment
( Assignment
, rule
, content
, children
, Rose(..)
, Node(..)
, AST
, assignAll
, runAssignment
) where

import Control.Monad.Free.Freer
import Prologue hiding (Alt)

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment symbol = Freer (AssignmentF symbol)

data AssignmentF symbol a where
  Rule :: symbol -> AssignmentF symbol ()
  Content :: AssignmentF symbol ByteString
  Children :: a -> AssignmentF symbol a
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
children forEach = Children forEach `Then` identity


-- | A rose tree.
data Rose a = Rose a [Rose a]
  deriving (Eq, Functor, Show)

-- | A node in the input AST. We only concern ourselves with its symbol (considered as an element of 'grammar') and content.
data Node grammar = Node { nodeSymbol :: grammar, nodeContent :: ByteString }
  deriving (Eq, Show)

-- | An abstract syntax tree.
type AST grammar = Rose (Node grammar)

-- | Run an assignment of nodes in a grammar onto terms in a syntax, discarding any unparsed nodes.
assignAll :: Eq grammar => Assignment grammar a -> [AST grammar] -> Maybe a
assignAll = (fmap snd .) . runAssignment

-- | Run an assignment of nodes in a grammar onto terms in a syntax.
runAssignment :: Eq grammar => Assignment grammar a -> [AST grammar] -> Maybe ([AST grammar], a)
runAssignment = iterFreer (\ assignment yield nodes -> case (assignment, nodes) of
  -- Nullability: some rules, e.g. 'pure a' and 'many a', should match at the end of input. Either side of an alternation may be nullable, ergo Alt can match at the end of input.
  (Alt a b, nodes) -> yield a nodes <|> yield b nodes -- FIXME: Rule `Alt` Rule `Alt` Rule is inefficient, should build and match against an IntMap instead.
  (assignment, Rose Node{..} children : rest) -> case assignment of
    Rule symbol ->
      if symbol == nodeSymbol then
        yield () nodes
      else
        Nothing
    Content -> yield nodeContent rest
    Children each -> fmap (first (const rest)) (yield each children)
    _ -> Nothing
  _ -> Nothing)
  . fmap ((Just .) . flip (,))


instance Alternative (Assignment symbol) where
  empty = Empty `Then` return
  (<|>) = (wrap .) . Alt

{-# LANGUAGE GADTs #-}
module Data.Syntax.Assignment where

import Control.Monad.Free.Freer
import Prologue hiding (Alt)

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment symbol = Freer (AssignmentF symbol)

data AssignmentF symbol a where
  Rule :: symbol -> a -> AssignmentF symbol a
  Content :: AssignmentF symbol ByteString
  Children :: a -> AssignmentF symbol a
  Alt :: a -> a -> AssignmentF symbol a
  Empty :: AssignmentF symbol a

-- | Match a node with the given symbol and apply a rule to it to parse it.
rule :: symbol -> Assignment symbol a -> Assignment symbol a
rule symbol = wrap . Rule symbol

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

-- | Small-step evaluation of an assignment from a grammar onto a syntax.
stepAssignment :: Eq grammar => Assignment grammar a -> [AST grammar] -> Maybe ([AST grammar], a)
stepAssignment = iterFreer (\ assignment yield nodes -> case (assignment, nodes) of
  -- Nullability: some rules, e.g. 'pure a' and 'many a', should match at the end of input. Either side of an alternation may be nullable, ergo Alt can match at the end of input.
  (Alt a b, nodes) -> yield a nodes <|> yield b nodes -- FIXME: Rule `Alt` Rule `Alt` Rule is inefficient, should build and match against an IntMap instead.
  (assignment, Rose Node{..} children : rest) -> case assignment of
    Rule symbol subRule ->
      if symbol == nodeSymbol then
        yield subRule nodes
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

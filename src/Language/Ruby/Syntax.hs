{-# LANGUAGE DataKinds, GADTs #-}
module Language.Ruby.Syntax where

import Control.Monad.Free.Freer
import Data.Functor.Union
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Prologue hiding (Alt)

-- | The type of Ruby syntax.
type Syntax = Union
  '[Comment.Comment
  , Declaration.Class
  , Declaration.Method
  , Literal.Boolean
  , Statement.If
  , Statement.Return
  , Statement.Yield
  , Syntax.Identifier
  ]

-- | Assignment from an AST with some set of 'symbol's onto some other value.
--
--   This is essentially a parser.
type Assignment symbol = Freer (AssignmentF symbol)

data AssignmentF symbol a where
  Rule :: symbol -> a -> AssignmentF symbol a
  Content :: AssignmentF symbol ByteString
  Children :: a -> AssignmentF symbol a
  Alt :: a -> a -> AssignmentF symbol a
  Fail :: AssignmentF symbol a

-- | Match a node with the given symbol and apply a rule to it to parse it.
rule :: symbol -> Assignment symbol a -> Assignment symbol a
rule symbol = wrap . Rule symbol

-- | A rule to produce a node’s content as a ByteString.
content :: Assignment symbol ByteString
content = Content `Then` return

-- | Match a node by applying an assignment to its children.
children :: Assignment symbol a -> Assignment symbol a
children forEach = Children forEach `Then` identity


-- | A program in some syntax functor, over which we can perform analyses.
type Program = Freer


-- | Statically-known rules corresponding to symbols in the grammar.
data Grammar = Program | Uninterpreted | BeginBlock | EndBlock | Undef | Alias | Comment | True' | False' | Return | Yield | Break | Next | Redo | Retry | IfModifier | UnlessModifier | WhileModifier | UntilModifier | RescueModifier | While | Until | For | Do | Case | When | Pattern | If | Unless | Elsif | Else | Begin | Ensure | Rescue | Exceptions | ExceptionVariable | ElementReference | ScopeResolution | Call | MethodCall | ArgumentList | ArgumentListWithParens | SplatArgument | HashSplatArgument | BlockArgument | Class | Constant | Method | Identifier
  deriving (Enum, Eq, Ord, Show)

-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment Grammar (Program Syntax (Maybe a))
assignment = foldr (>>) (pure Nothing) <$> rule Program (children (many declaration))
  where declaration = comment <|> class' <|> method
        class' = rule Class (wrapU <$> (Declaration.Class <$> constant <*> pure [] <*> declaration))
        constant = rule Constant (wrapU <$> (Syntax.Identifier <$> content))
        identifier = rule Identifier (wrapU <$> (Syntax.Identifier <$> content))
        method = rule Method (wrapU <$> (Declaration.Method <$> identifier <*> pure [] <*> statement))
        statement = expr

comment :: Assignment Grammar (Program Syntax a)
comment = wrapU . Comment.Comment <$> (rule Comment content)

if' :: Assignment Grammar (Program Syntax a)
if' = rule If (wrapU <$> (Statement.If <$> expr <*> expr <*> expr))

expr :: Assignment Grammar (Program Syntax a)
expr = if'


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
stepAssignment = iterFreer (\ assignment yield nodes -> case nodes of
  [] -> Nothing
  Rose Node{..} children : rest -> case assignment of
    Rule symbol subRule ->
      if symbol == nodeSymbol then
        yield subRule nodes
      else
        Nothing
    Content -> yield nodeContent rest
    Children each -> fmap (first (const rest)) (yield each children)
    Alt a b -> yield a nodes <|> yield b nodes
    -- FIXME: Rule `Alt` Rule `Alt` Rule is inefficient, should build and match against an IntMap instead.
    Fail -> Nothing) . fmap ((Just .) . flip (,))


instance Alternative (Assignment symbol) where
  empty = Fail `Then` return
  (<|>) = (wrap .) . Alt

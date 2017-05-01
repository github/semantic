{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
module Language.Ruby.Syntax where

import Data.Functor.Foldable (Base)
import Data.Functor.Union
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Syntax.Assignment
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import Language.Haskell.TH hiding (location, Range(..))
import Prologue hiding (for, get, Location, optional, state, unless)
import Term
import Text.Parser.TreeSitter.Language
import Text.Parser.TreeSitter.Ruby

-- | The type of Ruby syntax.
type Syntax = Union Syntax'
type Syntax' =
  '[Comment.Comment
  , Declaration.Class
  , Declaration.Method
  , Expression.Not
  , Literal.Array
  , Literal.Boolean
  , Literal.Hash
  , Literal.Integer
  , Literal.Range
  , Literal.String
  , Literal.Symbol
  , Statement.Break
  , Statement.Continue
  , Statement.ForEach
  , Statement.If
  -- TODO: redo
  -- TODO: retry
  , Statement.Return
  , Statement.While
  , Statement.Yield
  , Syntax.Empty
  , Syntax.Identifier
  , []
  ]


-- | Statically-known rules corresponding to symbols in the grammar.
mkSymbolDatatype (mkName "Grammar") tree_sitter_ruby


-- | Assignment from AST in Ruby’s grammar onto a program in Ruby’s syntax.
assignment :: Assignment (Node Grammar) [Term Syntax Location]
assignment = symbol Program *> children (many declaration)

declaration :: Assignment (Node Grammar) (Term Syntax Location)
declaration = comment <|> class' <|> method

class' :: Assignment (Node Grammar) (Term Syntax Location)
class' = symbol Class *> term <*> children (Declaration.Class <$> (constant <|> scopeResolution) <*> (superclass <|> pure []) <*> many declaration)
  where superclass = pure <$ symbol Superclass <*> children constant
        scopeResolution = symbol ScopeResolution *> children (constant <|> identifier)

constant :: Assignment (Node Grammar) (Term Syntax Location)
constant = leaf Constant Syntax.Identifier

identifier :: Assignment (Node Grammar) (Term Syntax Location)
identifier = leaf Identifier Syntax.Identifier

method :: Assignment (Node Grammar) (Term Syntax Location)
method = symbol Method *> term <*> children (Declaration.Method <$> identifier <*> pure [] <*> (term <*> many statement))

statement :: Assignment (Node Grammar) (Term Syntax Location)
statement  =  exit Statement.Return Return
          <|> exit Statement.Yield Yield
          <|> exit Statement.Break Break
          <|> exit Statement.Continue Next
          <|> if'
          <|> unless
          <|> while
          <|> until
          <|> for
          <|> literal
  where exit construct sym = symbol sym *> term <*> (children (construct <$> optional (symbol ArgumentList *> children statement)))

comment :: Assignment (Node Grammar) (Term Syntax Location)
comment = leaf Comment Comment.Comment

if' :: Assignment (Node Grammar) (Term Syntax Location)
if' =  go If
   <|> symbol IfModifier *> term <*> children (flip Statement.If <$> statement <*> statement <*> (term <*> pure Syntax.Empty))
  where go s = symbol s  *> term <*> children      (Statement.If <$> statement <*> (term <*> many statement) <*> optional (go Elsif <|> symbol Else *> term <*> children (many statement)))

unless :: Assignment (Node Grammar) (Term Syntax Location)
unless =  symbol Unless         *> term <*> children      (Statement.If <$> (term <*> (Expression.Not <$> statement)) <*> (term <*> many statement) <*> optional (symbol Else *> term <*> children (many statement)))
      <|> symbol UnlessModifier *> term <*> children (flip Statement.If <$> statement <*> (term <*> (Expression.Not <$> statement)) <*> (term <*> pure Syntax.Empty))

while :: Assignment (Node Grammar) (Term Syntax Location)
while =  symbol While         *> term <*> children      (Statement.While <$> statement <*> (term <*> many statement))
     <|> symbol WhileModifier *> term <*> children (flip Statement.While <$> statement <*> statement)

until :: Assignment (Node Grammar) (Term Syntax Location)
until =  symbol Until         *> term <*> children      (Statement.While <$> (term <*> (Expression.Not <$> statement)) <*> (term <*> many statement))
     <|> symbol UntilModifier *> term <*> children (flip Statement.While <$> statement <*> (term <*> (Expression.Not <$> statement)))

for :: Assignment (Node Grammar) (Term Syntax Location)
for = symbol For *> term <*> children (Statement.ForEach <$> identifier <*> statement <*> (term <*> many statement))

literal :: Assignment (Node Grammar) (Term Syntax Location)
literal  =  leaf Language.Ruby.Syntax.True (const Literal.true)
        <|> leaf Language.Ruby.Syntax.False (const Literal.false)
        <|> leaf Language.Ruby.Syntax.Integer Literal.Integer
        <|> symbol Range *> term <*> children (Literal.Range <$> statement <*> statement) -- FIXME: represent the difference between .. and ...

-- | Assignment of the current node’s annotation.
term :: InUnion Syntax' f => Assignment (Node grammar) (f (Term Syntax Location) -> Term Syntax Location)
term = (\ a f -> cofree $ a :< inj f) <$> location

leaf :: (Enum symbol, Eq symbol, InUnion Syntax' f) => symbol -> (ByteString -> f (Term Syntax Location)) -> Assignment (Node symbol) (Term Syntax Location)
leaf s f = (\ a -> cofree . (a :<) . inj . f) <$ symbol s <*> location <*> source

optional :: Assignment (Node Grammar) (Term Syntax Location) -> Assignment (Node Grammar) (Term Syntax Location)
optional a = a <|> term <*> pure Syntax.Empty


-- | An F-algebra on some carrier functor 'f'.
type FAlgebra f a = f a -> a

-- | An R-algebra on some carrier functor 'f' of its fixpoint type 't'.
type RAlgebra f t a = f (t, a) -> a

-- | Promote an FAlgebra into an RAlgebra (by dropping the original parameter).
fToR :: Functor (Base t) => FAlgebra (Base t) a -> RAlgebra (Base t) t a
fToR f = f . fmap snd

newtype Identifier' = Identifier' ByteString
  deriving (Eq, Show)

-- | Produce the identifier for a given term, if any.
--
--   Identifier syntax is labelled, as well as declaration syntax identified by these, but other uses of these identifiers are not, e.g. the declaration of a class or method or binding of a variable will be labelled, but a function call will not.
identifierAlg :: (InUnion fs Syntax.Identifier, InUnion fs Declaration.Method, InUnion fs Declaration.Class, Traversable (Union fs)) => FAlgebra (Base (Term (Union fs) a)) (Maybe Identifier')
identifierAlg (_ :< union) = case union of
  _ | Just (Syntax.Identifier s) <- prj union -> Just (Identifier' s)
  _ | Just Declaration.Class{..} <- prj union -> classIdentifier
  _ | Just Declaration.Method{..} <- prj union -> methodName
  _ -> Nothing

-- | The cyclomatic complexity of a (sub)term.
newtype CyclomaticComplexity = CyclomaticComplexity Int
  deriving (Enum, Eq, Num, Ord, Show)

-- | Compute the cyclomatic complexity of a (sub)term, measured as the number places where control exits scope, e.g. returns and yields.
--
--   TODO: Explicit returns at the end of methods should only count once.
--   TODO: Anonymous functions should not increase parent scope’s complexity.
--   TODO: Inner functions should not increase parent scope’s complexity.
cyclomaticComplexityAlg :: (InUnion fs Declaration.Method, InUnion fs Statement.Return, InUnion fs Statement.Yield, Traversable (Union fs)) => FAlgebra (Base (Term (Union fs) a)) CyclomaticComplexity
cyclomaticComplexityAlg (_ :< union) = case union of
  _ | Just Declaration.Method{} <- prj union -> succ (sum union)
  _ | Just Statement.Return{} <- prj union -> succ (sum union)
  _ | Just Statement.Yield{} <- prj union -> succ (sum union)
  _ -> sum union

-- | Lift an algebra into a decorator for terms annotated with records.
decoratorWithAlgebra :: Functor f
                     => RAlgebra (Base (Term f (Record fs))) (Term f (Record fs)) a -- ^ An F-algebra on terms.
                     -> Term f (Record fs) -- ^ A term to decorate with values produced by the F-algebra.
                     -> Term f (Record (a ': fs)) -- ^ A term decorated with values produced by the F-algebra.
decoratorWithAlgebra alg = para $ \ c@(a :< f) -> cofree $ (alg (fmap (second (rhead . extract)) c) :. a) :< fmap snd f

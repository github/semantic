{-# LANGUAGE ConstraintKinds, DataKinds, DefaultSignatures, DisambiguateRecordFields, FlexibleContexts,
             GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, NamedFieldPuns, OverloadedLists,
             OverloadedStrings, PatternSynonyms, StandaloneDeriving, TypeApplications, TypeOperators, ViewPatterns #-}

module Language.Python.Core
( toplevelCompile
, Bindings
) where

import Prelude hiding (fail)

import           AST.Element
import           Control.Effect hiding ((:+:))
import           Control.Effect.Reader
import           Control.Monad.Fail
import           Core.Core as Core
import           Core.Name as Name
import           Data.Coerce
import           Data.Foldable
import           Data.Function
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Traversable
import           GHC.Records
import           Source.Span (Span)
import           Syntax.Stack (Stack (..))
import qualified Syntax.Stack as Stack
import qualified TreeSitter.Python.AST as Py

-- | Keeps track of the current scope's bindings (so that we can, when
-- compiling a class or module, return the list of bound variables as
-- a Core record so that all immediate definitions are exposed)
newtype Bindings = Bindings { unBindings :: Stack Name }
  deriving (Eq, Monoid, Semigroup, Show)

def :: Name -> Bindings -> Bindings
def n = coerce (Stack.:> n)

-- | Useful pattern synonym for extracting a single identifier from
-- a Python ExpressionList. Easier than pattern-matching every time.
-- TODO: when this is finished, we won't need this pattern, as we'll
-- handle ExpressionLists the smart way every time.
pattern SingleIdentifier :: Name -> Py.ExpressionList a
pattern SingleIdentifier name <- Py.ExpressionList
  { Py.extraChildren =
    [ Py.Expression (Prj (Py.PrimaryExpression (Prj Py.Identifier { text = Name -> name })))
    ]
  }

-- We leave the representation of Core syntax abstract so that it's not
-- possible for us to 'cheat' by pattern-matching on or eliminating a
-- compiled term.
type CoreSyntax sig t = ( Member Core sig
                        , Member (Ann Span) sig
                        , Carrier sig t
                        , Foldable t
                        )

class Compile (py :: * -> *) where
  -- FIXME: rather than failing the compilation process entirely
  -- with MonadFail, we should emit core that represents failure
  compile :: ( CoreSyntax syn t
             , Member (Reader Bindings) sig
             , Carrier sig m
             , MonadFail m
             )
          => py Span
          -> (t Name -> m (t Name))
          -> (t Name -> m (t Name))

  default compile :: (MonadFail m, Show (py Span)) => py Span -> (t Name -> m (t Name)) -> (t Name -> m (t Name))
  compile a _ _ = defaultCompile a

toplevelCompile :: ( CoreSyntax syn t
                   , Member (Reader Bindings) sig
                   , Carrier sig m
                   , MonadFail m
                   )
                => Py.Module Span
                -> m (t Name)
toplevelCompile py = compile py pure none

-- | TODO: This is not right, it should be a reference to a Preluded
-- NoneType instance, but it will do for now.
none :: (Member Core sig, Carrier sig t) => t Name
none = unit

locate :: ( HasField "ann" syntax Span
          , CoreSyntax syn t
          )
       => syntax
       -> t a
       -> t a
locate syn  = Core.annAt (getField @"ann" syn)

defaultCompile :: (MonadFail m, Show py) => py -> m (t Name)
defaultCompile t = fail $ "compilation unimplemented for " <> show t


instance (Compile l, Compile r) => Compile (l :+: r) where
  compile (L1 l) cc = compile l cc
  compile (R1 r) cc = compile r cc

instance Compile Py.AssertStatement
instance Compile Py.Attribute

-- Assignment compilation. Assignments are an uneasy hybrid of expressions
-- (since they appear to have values, i.e. `a = b = c`) and statements (because
-- they introduce bindings). For that reason, they deserve special attention.
--
-- The correct desugaring for the expression above looks like, given a continuation @cont@:
-- @
--  (b :<- c) >>>= (a :<- b) >>>= cont
-- @
-- The tree structure that we get out of tree-sitter is not particulary conducive to expressing
-- this naturally, so we engage in a small desugaring step so that we can turn a list [a, b, c]
-- into a sequenced Core expression using >>>= and a fold through which information—specifically
-- the LHS to assign—flows.

-- RHS represents the right-hand-side of an assignment that we get out of tree-sitter.
-- Desugared is the "terminal" node in a sequence of assignments, i.e. given a = b = c,
-- c will be the terminal node. It is never an assignment.
type RHS = (Py.Assignment :+: Py.AugmentedAssignment) :+: Desugared
type Desugared = Py.ExpressionList :+: Py.Yield

-- We have to pair locations and names, and tuple syntax is harder to
-- read in this case than a happy little constructor.
data Located a = Located Span a

-- Desugaring an RHS involves walking as deeply as possible into an
-- assignment, storing the names we encounter as we go and eventually
-- returning a terminal expression. We have to keep track of which
desugar :: MonadFail m
        => [Located Name]
        -> RHS Span
        -> m ([Located Name], Desugared Span)
desugar acc = \case
  Prj Py.Assignment { left = SingleIdentifier name, right = Just rhs, ann} ->
    desugar (Located ann name : acc) rhs
  R1 any -> pure (acc, any)
  other -> fail ("desugar: couldn't desugar RHS " <> show other)

-- This is an algebra that is invoked from a left fold but that
-- returns a function (the 'difference' pattern) so that we can pass
-- information about what RHS we need down the chain: unlike most fold
-- functions, it has four parameters, not three (since our fold
-- returns a function). There's some pun to be made on "collapsing
-- sugar", like "icing" or "sugar water" but I'll leave that as an
-- exercise to the reader.
collapseDesugared :: (CoreSyntax syn t, Member (Reader Bindings) sig, Carrier sig m)
                  => Located Name           -- The current LHS to which to assign
                  -> (t Name -> m (t Name)) -- A meta-continuation: it takes a name and returns a continuation
                  -> t Name                 -- The current RHS to which to assign, yielded from an outer continuation
                  -> m (t Name)             -- The properly-sequenced resolut
collapseDesugared (Located loc n) cont rem =
  let assigning = fmap (Core.annAt loc . ((Name.named' n :<- rem) >>>=))
  in assigning (local (def n) (cont (pure n))) -- gotta call local here to record this assignment

instance Compile Py.Assignment where
  compile it@Py.Assignment
    { left = SingleIdentifier name
    , right = Just rhs
    , ann
    } cc next = do
    (names, val) <- desugar [Located ann name] rhs
    compile val pure next >>= foldr collapseDesugared cc names >>= pure . locate it

  compile other _ _ = fail ("Unhandled assignment case: " <> show other)

-- End assignment compilation

instance Compile Py.AugmentedAssignment
instance Compile Py.Await
instance Compile Py.BinaryOperator

instance Compile Py.Block where
  compile it@Py.Block{ Py.extraChildren = body} cc
    = fmap (locate it)
    . foldr compile cc body

instance Compile Py.BooleanOperator
instance Compile Py.BreakStatement

instance Compile Py.Call where
  compile it@Py.Call
    { function
    , arguments = L1 Py.ArgumentList { extraChildren = args }
    } cc next = do
    func <- compile function pure next
    let compileArg = \case
          Prj expr -> compile (expr :: Py.Expression Span) pure next
          other   -> fail ("Can't compile non-expression function argument: " <> show other)

    -- Python function arguments are defined to evaluate left to right.
    args <- traverse compileArg args
    locate it (func $$* args) & cc
  compile it _ _ = fail ("can't compile Call node with generator expression: " <> show it)

instance Compile Py.ClassDefinition where
  compile it@Py.ClassDefinition { body = pybody, name = Py.Identifier _ann (Name -> n) } cc next = do
    let buildTypeCall _ = do
          bindings <- asks @Bindings (toList . unBindings)
          let buildName n = (n, pure n)
              contents = record . fmap buildName $ bindings
              typefn = pure "__semantic_prelude" ... "type"
              object = pure "__semantic_prelude" ... "object"

          pure (typefn $$ Core.string (coerce n) $$ object $$ contents)

    body <- compile pybody buildTypeCall next
    let coreName = Name.named' n
        assignClass = coreName :<- (rec coreName body)
        continuing = fmap (locate it . (assignClass >>>=))
    continuing (local (def n) (cc next))

instance Compile Py.ComparisonOperator

deriving instance Compile Py.CompoundStatement

instance Compile Py.ConcatenatedString
instance Compile Py.ConditionalExpression
instance Compile Py.ContinueStatement

instance Compile Py.DecoratedDefinition where
  compile it@Py.DecoratedDefinition
    { definition
    , extraChildren = [ Py.Decorator { extraChildren } ]
    } cc next = do
    let thenReassign item = do
          _ :> lastbound <- asks unBindings
          tocall <- compile extraChildren pure next
          let callit go = (pure lastbound .= (tocall $$ pure lastbound)) >>> go
          fmap callit (cc item)
    locate it <$> compile definition thenReassign next
  compile it _ _ = fail ("Can't figure out decorated definition " <> show it)
instance Compile Py.DeleteStatement
instance Compile Py.Dictionary
instance Compile Py.DictionaryComprehension

instance Compile Py.DottedName where
  compile it@Py.DottedName
    { extraChildren = Py.Identifier { text } :| rest
    } cc _next = do
    let aggregate Py.Identifier { text = inner } x = x ... Name inner
        composite = foldr aggregate (pure (Name text)) rest
    locate it composite & cc


instance Compile Py.Ellipsis
instance Compile Py.ExecStatement

deriving instance Compile Py.Expression

instance Compile Py.ExpressionStatement where
  compile it@Py.ExpressionStatement { Py.extraChildren = children } cc
    = fmap (locate it)
    . foldr compile cc children

instance Compile Py.ExpressionList where
  compile it@Py.ExpressionList { Py.extraChildren = [child] } cc
    = fmap (locate it)
    . compile child cc
  compile Py.ExpressionList { Py.extraChildren = items } _
    = const (fail ("unimplemented: ExpressionList of length " <> show items))


instance Compile Py.False where
  compile it cc _ = cc $ locate it (bool False)

instance Compile Py.Float
instance Compile Py.ForStatement

instance Compile Py.FunctionDefinition where
  compile it@Py.FunctionDefinition
    { name       = Py.Identifier _ann1 name
    , parameters = Py.Parameters _ann2 parameters
    , body
    } cc next = do
      -- Compile each of the parameters, then the body.
      parameters' <- traverse param parameters
      body' <- compile body pure next
      -- Build a lambda.
      let located = locate it (rec (Name.named' (Name name)) (lams parameters' body'))
      -- Give it a name (below), then augment the current continuation
      -- with the new name (with 'def'), so that calling contexts know
      -- that we have built an exportable definition.
      assigning located <$> local (def (Name name)) (cc next)
    where param (Py.Parameter (Prj (Py.Identifier _pann pname))) = pure . named' . Name $ pname
          param x                                                = unimplemented x
          unimplemented x = fail $ "unimplemented: " <> show x
          assigning item f = (Name.named' (Name name) :<- item) >>>= f

instance Compile Py.FutureImportStatement
instance Compile Py.GeneratorExpression
instance Compile Py.GlobalStatement

instance Compile Py.Identifier where
  compile Py.Identifier { text } cc _ = cc . pure . Name $ text

instance Compile Py.IfStatement where
  compile it@Py.IfStatement{ condition, consequence, alternative} cc next =
    locate it <$> (if' <$> compile condition pure next
                       <*> compile consequence cc next
                       <*> foldr clause (cc next) alternative)
    where clause (R1 Py.ElseClause{ body }) _ = compile body cc next
          clause (L1 Py.ElifClause{ condition, consequence }) rest  =
            if' <$> compile condition pure next <*> compile consequence cc next <*> rest


instance Compile Py.ImportFromStatement
instance Compile Py.ImportStatement
instance Compile Py.Integer

instance Compile Py.Lambda
instance Compile Py.List
instance Compile Py.ListComprehension

instance Compile Py.Module where
  compile it@Py.Module { Py.extraChildren = stmts } _cc =
    -- This action gets passed to compile, which means it is the
    -- final action taken after the compiling fold finishes. It takes
    -- care of listening for the current set of bound variables (which
    -- is augmented by assignments and function definitions) and
    -- creating a record corresponding to those bindings.
    let buildRecord _ = do
          bindings <- asks @Bindings (toList . unBindings)
          let buildName n = (n, pure n)
          pure . record . fmap buildName $ bindings
    in fmap (locate it) . foldr compile buildRecord stmts

instance Compile Py.NamedExpression
instance Compile Py.None
instance Compile Py.NonlocalStatement
instance Compile Py.NotOperator

instance Compile Py.ParenthesizedExpression where
  compile it@Py.ParenthesizedExpression { extraChildren } cc
    = fmap (locate it)
    . compile extraChildren cc

instance Compile Py.PassStatement where
  compile it@Py.PassStatement {} cc _ = cc $ locate it Core.unit

deriving instance Compile Py.PrimaryExpression

instance Compile Py.PrintStatement

instance Compile Py.ReturnStatement where
  compile it@Py.ReturnStatement { Py.extraChildren = vals } _ next = locate it <$> case vals of
    Nothing -> pure none
    Just Py.ExpressionList { extraChildren = [val] } -> compile val pure next
    Just Py.ExpressionList { extraChildren = vals  } -> fail ("unimplemented: return statement returning " <> show (length vals) <> " values")


instance Compile Py.RaiseStatement
instance Compile Py.Set
instance Compile Py.SetComprehension

deriving instance Compile Py.SimpleStatement

instance Compile Py.String where
  compile it@Py.String { extraChildren } cc _ = do
    contents <- for extraChildren $ \case
      Prj Py.EscapeSequence { text } -> pure text
      other                          -> fail ("Couldn't string-desugar " <> show other)

    let new = pure "__semantic_prelude" ... "str" ... "__slots" ... "__new__"
    cc $ locate it (new $$ Core.string (mconcat contents))

instance Compile Py.Subscript

instance Compile Py.True where
  compile it cc _next = cc $ locate it (bool True)

instance Compile Py.TryStatement

instance Compile Py.Tuple where
  compile it@Py.Tuple { Py.extraChildren = [] } cc _ = cc $ locate it unit

  compile it _ _
    = fail ("Unimplemented: non-empty tuple " <> show it)

instance Compile Py.UnaryOperator
instance Compile Py.WhileStatement
instance Compile Py.WithStatement
instance Compile Py.Yield

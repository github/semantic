{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, DataKinds, DefaultSignatures, DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             DerivingVia, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             KindSignatures, LambdaCase, NamedFieldPuns, OverloadedLists, OverloadedStrings, PatternSynonyms,
             ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}

module Language.Python.Core
( compile
, Bindings
, SourcePath
) where

import Prelude hiding (fail)

import           Control.Effect hiding ((:+:))
import           Control.Effect.Reader
import           Control.Monad.Fail
import           Data.Coerce
import           Data.Core as Core
import           Data.Foldable
import           Data.Loc (Loc)
import qualified Data.Loc
import           Data.Name as Name
import           Data.Stack (Stack)
import qualified Data.Stack as Stack
import           Data.String (IsString)
import           Data.Text (Text)
import           GHC.Generics
import           GHC.Records
import           Source.Span (Span)
import qualified Source.Span as Source
import qualified TreeSitter.Python.AST as Py

-- | Access to the current filename as Text to stick into location annotations.
newtype SourcePath = SourcePath { rawPath :: Text }
  deriving stock (Eq, Show)
  deriving newtype IsString

-- | Keeps track of the current scope's bindings (so that we can, when
-- compiling a class or module, return the list of bound variables as
-- a Core record so that all immediate definitions are exposed)
newtype Bindings = Bindings { unBindings :: Stack Name }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

def :: Name -> Bindings -> Bindings
def n = coerce (Stack.:> n)

-- | Useful pattern synonym for extracting a single identifier from
-- a Python ExpressionList. Easier than pattern-matching every time.
-- TODO: when this is finished, we won't need this pattern, as we'll
-- handle ExpressionLists the smart way every time.
pattern SingleIdentifier :: Name -> Py.ExpressionList a
pattern SingleIdentifier name <- Py.ExpressionList
  { Py.extraChildren =
    [ Py.PrimaryExpressionExpression (Py.IdentifierPrimaryExpression (Py.Identifier { bytes = (Name -> name) }))
    ]
  }

-- We leave the representation of Core syntax abstract so that it's not
-- possible for us to 'cheat' by pattern-matching on or eliminating a
-- compiled term.
type CoreSyntax sig t = ( Member Core sig
                        , Member Ann sig
                        , Carrier sig t
                        , Foldable t
                        )

class Compile (py :: * -> *) where
  -- FIXME: rather than failing the compilation process entirely
  -- with MonadFail, we should emit core that represents failure
  compileCC :: ( CoreSyntax syn t
               , Member (Reader SourcePath) sig
               , Member (Reader Bindings) sig
               , Carrier sig m
               , MonadFail m
               )
            => py Span
            -> m (t Name)
            -> m (t Name)

  default compileCC :: (MonadFail m, Show (py Span)) => py Span -> m (t Name) -> m (t Name)
  compileCC a _ = defaultCompile a

-- | TODO: This is not right, it should be a reference to a Preluded
-- NoneType instance, but it will do for now.
none :: (Member Core sig, Carrier sig t) => t Name
none = unit

compile :: ( Compile py
           , CoreSyntax syn t
           , Member (Reader SourcePath) sig
           , Member (Reader Bindings) sig
           , Carrier sig m
           , MonadFail m
           )
        => py Span
        -> m (t Name)
compile t = compileCC t (pure none)

locFromTSSpan :: SourcePath -> Source.Span -> Loc
locFromTSSpan fp (Source.Span (Source.Pos a b) (Source.Pos c d))
  = Data.Loc.Loc (rawPath fp) (Data.Loc.Span (Data.Loc.Pos a b) (Data.Loc.Pos c d))

locate :: ( HasField "ann" syntax Span
          , CoreSyntax syn t
          , Member (Reader SourcePath) sig
          , Carrier sig m
          )
       => syntax
       -> t a
       -> m (t a)
locate syn item = do
  fp <- ask @SourcePath
  pure (Core.annAt (locFromTSSpan fp (getField @"ann" syn)) item)

defaultCompile :: (MonadFail m, Show py) => py -> m (t Name)
defaultCompile t = fail $ "compilation unimplemented for " <> show t

newtype CompileSum py a = CompileSum (py a)

instance (Generic1 py, GCompileSum (Rep1 py)) => Compile (CompileSum py) where
  compileCC (CompileSum a) cc = gcompileCCSum (from1 a) cc

deriving via CompileSum (l :+: r) instance (Compile l, Compile r) => Compile (l :+: r)

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
type RHS = Py.Assignment :+: Py.AugmentedAssignment :+: Desugared
type Desugared = Py.ExpressionList :+: Py.Yield

-- We have to pair locations and names, and tuple syntax is harder to
-- read in this case than a happy little constructor.
data Located a = Located Loc a

-- Desugaring an RHS involves walking as deeply as possible into an
-- assignment, storing the names we encounter as we go and eventually
-- returning a terminal expression. We have to keep track of which
desugar :: (Member (Reader SourcePath) sig, Carrier sig m, MonadFail m)
        => [Located Name]
        -> RHS Span
        -> m ([Located Name], Desugared Span)
desugar acc = \case
  L1 Py.Assignment { left = SingleIdentifier name, right = Just rhs, ann} -> do
    loc <- locFromTSSpan <$> ask <*> pure ann
    let cons = (Located loc name :)
    desugar (cons acc) rhs
  R1 (R1 any) -> pure (acc, any)
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
  compileCC it@Py.Assignment
    { left = SingleIdentifier name
    , right = Just rhs
    , ann
    } cc = do
    p <- ask @SourcePath
    (names, val) <- desugar [Located (locFromTSSpan p ann) name] rhs
    compile val >>= foldr collapseDesugared (const cc) names >>= locate it

  compileCC other _ = fail ("Unhandled assignment case: " <> show other)

-- End assignment compilation

instance Compile Py.AugmentedAssignment
instance Compile Py.Await
instance Compile Py.BinaryOperator

instance Compile Py.Block where
  compileCC it@Py.Block{ Py.extraChildren = body} cc = locate it =<< foldr compileCC cc body

instance Compile Py.BooleanOperator
instance Compile Py.BreakStatement
instance Compile Py.Call
instance Compile Py.ClassDefinition
instance Compile Py.ComparisonOperator

deriving via CompileSum Py.CompoundStatement instance Compile Py.CompoundStatement

instance Compile Py.ConcatenatedString
instance Compile Py.ConditionalExpression
instance Compile Py.ContinueStatement
instance Compile Py.DecoratedDefinition
instance Compile Py.DeleteStatement
instance Compile Py.Dictionary
instance Compile Py.DictionaryComprehension
instance Compile Py.Ellipsis
instance Compile Py.ExecStatement

deriving via CompileSum Py.Expression instance Compile Py.Expression

instance Compile Py.ExpressionStatement where
  compileCC it@Py.ExpressionStatement
    { Py.extraChildren = children
    } cc = do
    foldr compileCC cc children >>= locate it

instance Compile Py.ExpressionList where
  compileCC it@Py.ExpressionList { Py.extraChildren = [child] } cc
    = compileCC child cc >>= locate it
  compileCC Py.ExpressionList { Py.extraChildren = items } _
    = fail ("unimplemented: ExpressionList of length " <> show items)


instance Compile Py.False where
  compileCC it _ = locate it $ bool False

instance Compile Py.Float
instance Compile Py.ForStatement

instance Compile Py.FunctionDefinition where
  compileCC it@Py.FunctionDefinition
    { name       = Py.Identifier _ann1 name
    , parameters = Py.Parameters _ann2 parameters
    , body
    } cc = do
      -- Compile each of the parameters, then the body.
      parameters' <- traverse param parameters
      body' <- compile body
      -- Build a lambda.
      located <- locate it (lams parameters' body')
      -- Give it a name (below), then augment the current continuation
      -- with the new name (with 'def'), so that calling contexts know
      -- that we have built an exportable definition.
      assigning located <$> local (def (Name name)) cc
    where param (Py.IdentifierParameter (Py.Identifier _pann pname)) = pure . named' . Name $ pname
          param x                                                    = unimplemented x
          unimplemented x = fail $ "unimplemented: " <> show x
          assigning item f = (Name.named' (Name name) :<- item) >>>= f

instance Compile Py.FutureImportStatement
instance Compile Py.GeneratorExpression
instance Compile Py.GlobalStatement

instance Compile Py.Identifier where
  compileCC Py.Identifier { bytes } _ = pure . pure . Name $ bytes

instance Compile Py.IfStatement where
  compileCC it@Py.IfStatement{ condition, consequence, alternative} cc =
    locate it =<< (if' <$> compile condition <*> compileCC consequence cc <*> foldr clause cc alternative)
    where clause (R1 Py.ElseClause{ body }) _ = compileCC body cc
          clause (L1 Py.ElifClause{ condition, consequence }) rest  =
            if' <$> compile condition <*> compileCC consequence cc <*> rest


instance Compile Py.ImportFromStatement
instance Compile Py.ImportStatement
instance Compile Py.Integer
instance Compile Py.Lambda
instance Compile Py.List
instance Compile Py.ListComprehension

instance Compile Py.Module where
  compileCC it@Py.Module { Py.extraChildren = stmts } _cc = do
    -- This action gets passed to compileCC, which means it is the
    -- final action taken after the compiling fold finishes. It takes
    -- care of listening for the current set of bound variables (which
    -- is augmented by assignments and function definitions) and
    -- creating a record corresponding to those bindings.
    let buildRecord = do
          bindings <- asks @Bindings (toList . unBindings)
          let buildName n = (n, pure n)
          pure . record . fmap buildName $ bindings
    foldr compileCC buildRecord stmts >>= locate it

instance Compile Py.NamedExpression
instance Compile Py.None
instance Compile Py.NonlocalStatement
instance Compile Py.NotOperator
instance Compile Py.ParenthesizedExpression

instance Compile Py.PassStatement where
  compileCC it@Py.PassStatement {} _ = locate it $ Core.unit

deriving via CompileSum Py.PrimaryExpression instance Compile Py.PrimaryExpression

instance Compile Py.PrintStatement

instance Compile Py.ReturnStatement where
  compileCC it@Py.ReturnStatement { Py.extraChildren = vals } _ = case vals of
    Nothing -> locate it $ none
    Just Py.ExpressionList { extraChildren = [val] } -> compile val >>= locate it
    Just Py.ExpressionList { extraChildren = vals  } -> fail ("unimplemented: return statement returning " <> show (length vals) <> " values")


instance Compile Py.RaiseStatement
instance Compile Py.Set
instance Compile Py.SetComprehension

deriving via CompileSum Py.SimpleStatement instance Compile Py.SimpleStatement

instance Compile Py.String
instance Compile Py.Subscript

instance Compile Py.True where
  compileCC it _ = locate it $ bool True

instance Compile Py.TryStatement

instance Compile Py.Tuple where
  compileCC it@Py.Tuple { Py.extraChildren = [] } _ = locate it unit

  compileCC it _
    = fail ("Unimplemented: non-empty tuple " <> show it)

instance Compile Py.UnaryOperator
instance Compile Py.WhileStatement
instance Compile Py.WithStatement
instance Compile Py.Yield

class GCompileSum (f :: * -> *) where
  gcompileCCSum :: ( CoreSyntax syn t
                   , Member (Reader SourcePath) sig
                   , Member (Reader Bindings) sig
                   , Carrier sig m
                   , MonadFail m
                   )
                => f Span
                -> m (t Name)
                -> m (t Name)

instance GCompileSum f => GCompileSum (M1 t d f) where
  gcompileCCSum (M1 f) = gcompileCCSum f

instance (GCompileSum l, GCompileSum r) => GCompileSum (l :+: r) where
  gcompileCCSum (L1 l) = gcompileCCSum l
  gcompileCCSum (R1 r) = gcompileCCSum r

instance Compile t => GCompileSum (Rec1 t) where
  gcompileCCSum (Rec1 t) = compileCC t

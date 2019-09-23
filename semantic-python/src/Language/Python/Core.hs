{-# LANGUAGE ConstraintKinds, DataKinds, DefaultSignatures, DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             DerivingVia, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             NamedFieldPuns, OverloadedLists, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving,
             TypeApplications, TypeOperators, UndecidableInstances #-}

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
import qualified Data.Loc
import           Data.Name as Name
import           Data.Stack (Stack)
import qualified Data.Stack as Stack
import           Data.String (IsString)
import           Data.Text (Text)
import           GHC.Generics
import           GHC.Records
import qualified TreeSitter.Python.AST as Py
import           TreeSitter.Span (Span)
import qualified TreeSitter.Span as TreeSitter

newtype SourcePath = SourcePath { rawPath :: Text }
  deriving stock (Eq, Show)
  deriving newtype IsString

-- Keeps track of the current scope's bindings (so that we can, when
-- compiling a class or module, return the list of bound variables
-- as a Core record so that all immediate definitions are exposed)
newtype Bindings = Bindings { unBindings :: Stack Name }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

def :: Name -> Bindings -> Bindings
def n = coerce (Stack.:> n)

-- We leave the representation of Core syntax abstract so that it's not
-- possible for us to 'cheat' by pattern-matching on or eliminating a
-- compiled term.
type CoreSyntax sig t = ( Member Core sig
                        , Member Ann sig
                        , Carrier sig t
                        , Foldable t
                        )

class Compile py where
  -- Should this go away, and should compileCC be the main function to call?
  compile :: ( CoreSyntax syn t
             , Member (Reader SourcePath) sig
             , Member (Reader Bindings) sig
             , Carrier sig m
             , MonadFail m
             )
          => py
          -> m (t Name)

  default compile :: (MonadFail m, Show py) => py -> m (t Name)
  compile = defaultCompile

  compileCC :: ( CoreSyntax syn t
               , Member (Reader SourcePath) sig
               , Member (Reader Bindings) sig
               , Carrier sig m
               , MonadFail m
               )
            => py
            -> m (t Name)
            -> m (t Name)
  compileCC py cc = (>>>) <$> compile py <*> cc


locate :: ( HasField "ann" syntax Span
          , CoreSyntax syn t
          , Member (Reader SourcePath) sig
          , Carrier sig m
          ) => syntax -> t a -> m (t a)
locate syn item = do
  fp <- asks @SourcePath rawPath
  let locFromTSSpan (TreeSitter.Span (TreeSitter.Pos a b) (TreeSitter.Pos c d))
        = Data.Loc.Loc fp (Data.Loc.Span (Data.Loc.Pos a b) (Data.Loc.Pos c d))

  pure (Core.annAt (locFromTSSpan (getField @"ann" syn)) item)

-- | TODO: This is not right, it should be a reference to a Preluded
-- NoneType instance, but it will do for now.
none :: (Member Core sig, Carrier sig t) => t Name
none = unit

-- | Helper for delegating to compileCC. The presence of this function indicates
-- that we might want to move 'compile' out of the Compile class entirely.
viaCompileCC :: ( Compile py
                , CoreSyntax syn t
                , Member (Reader SourcePath) sig
                , Member (Reader Bindings) sig
                , Carrier sig m
                , MonadFail m
                )
             => py -> m (t Name)
viaCompileCC t = compileCC t (pure none)

defaultCompile :: (MonadFail m, Show py) => py -> m (t Name)
defaultCompile t = fail $ "compilation unimplemented for " <> show t

newtype CompileSum py = CompileSum py

instance (Generic py, GCompileSum (Rep py)) => Compile (CompileSum py) where
  compile (CompileSum a) = gcompileSum . from $ a
  compileCC (CompileSum a) cc = gcompileCCSum (from a) cc

deriving via CompileSum (Either l r) instance (Compile l, Compile r) => Compile (Either l r)

instance Compile (Py.AssertStatement Span)
instance Compile (Py.Attribute Span)

instance Compile (Py.Assignment Span) where
  compileCC it@Py.Assignment
    { Py.left = Py.ExpressionList
      { Py.extraChildren =
        [ Py.PrimaryExpressionExpression (Py.IdentifierPrimaryExpression (Py.Identifier { Py.bytes = name }))
        ]
      }
    , Py.right = Just rhs
    } cc = do
    value  <- compile rhs
    let assigning n = (Name.named' name :<- value) >>>= n
    locate it =<< assigning <$> local (def name) cc
  compileCC other _ = fail ("Unhandled assignment case: " <> show other)

  compile = viaCompileCC

instance Compile (Py.AugmentedAssignment Span)
instance Compile (Py.Await Span)
instance Compile (Py.BinaryOperator Span)

instance Compile (Py.Block Span) where
  compileCC it@Py.Block{ Py.extraChildren = body} cc = locate it =<< foldr compileCC cc body

  compile = viaCompileCC

instance Compile (Py.BooleanOperator Span)
instance Compile (Py.BreakStatement Span)
instance Compile (Py.Call Span)
instance Compile (Py.ClassDefinition Span)
instance Compile (Py.ComparisonOperator Span)

deriving via CompileSum (Py.CompoundStatement Span) instance Compile (Py.CompoundStatement Span)

instance Compile (Py.ConcatenatedString Span)
instance Compile (Py.ConditionalExpression Span)
instance Compile (Py.ContinueStatement Span)
instance Compile (Py.DecoratedDefinition Span)
instance Compile (Py.DeleteStatement Span)
instance Compile (Py.Dictionary Span)
instance Compile (Py.DictionaryComprehension Span)
instance Compile (Py.Ellipsis Span)
instance Compile (Py.ExecStatement Span)

deriving via CompileSum (Py.Expression Span) instance Compile (Py.Expression Span)

instance Compile (Py.ExpressionStatement Span) where
  compileCC it@Py.ExpressionStatement
    { Py.extraChildren = children
    } cc = do
    foldr compileCC cc children >>= locate it
  compile = viaCompileCC

instance Compile (Py.ExpressionList Span) where
  compile it@Py.ExpressionList { Py.extraChildren = [child] } = compile child >>= locate it
  compile Py.ExpressionList { Py.extraChildren = items } = fail ("unimplemented: ExpressionList of length " <> show items)


instance Compile (Py.False Span) where compile it = locate it $ bool False

instance Compile (Py.Float Span)
instance Compile (Py.ForStatement Span)

instance Compile (Py.FunctionDefinition Span) where
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
      assigning located <$> local (def name) cc
    where param (Py.IdentifierParameter (Py.Identifier _pann pname)) = pure (named' pname)
          param x                                                    = unimplemented x
          unimplemented x = fail $ "unimplemented: " <> show x
          assigning item f = (Name.named' name :<- item) >>>= f

  compile = viaCompileCC

instance Compile (Py.FutureImportStatement Span)
instance Compile (Py.GeneratorExpression Span)
instance Compile (Py.GlobalStatement Span)

instance Compile (Py.Identifier Span) where
  compile Py.Identifier { bytes } = pure (pure bytes)

instance Compile (Py.IfStatement Span) where
  compileCC it@Py.IfStatement{ condition, consequence, alternative} cc =
    locate it =<< (if' <$> compile condition <*> compileCC consequence cc <*> foldr clause cc alternative)
    where clause (Right Py.ElseClause{ body }) _ = compileCC body cc
          clause (Left  Py.ElifClause{ condition, consequence }) rest  =
            if' <$> compile condition <*> compileCC consequence cc <*> rest

  compile = viaCompileCC


instance Compile (Py.ImportFromStatement Span)
instance Compile (Py.ImportStatement Span)
instance Compile (Py.Integer Span)
instance Compile (Py.Lambda Span)
instance Compile (Py.List Span)
instance Compile (Py.ListComprehension Span)

instance Compile (Py.Module Span) where
  compile it@Py.Module { Py.extraChildren = stmts } = do
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

instance Compile (Py.NamedExpression Span)
instance Compile (Py.None Span)
instance Compile (Py.NonlocalStatement Span)
instance Compile (Py.NotOperator Span)
instance Compile (Py.ParenthesizedExpression Span)

instance Compile (Py.PassStatement Span) where
  compile it@Py.PassStatement {} = locate it $ Core.unit

deriving via CompileSum (Py.PrimaryExpression Span) instance Compile (Py.PrimaryExpression Span)

instance Compile (Py.PrintStatement Span)

instance Compile (Py.ReturnStatement Span) where
  compile it@Py.ReturnStatement { Py.extraChildren = vals } = case vals of
    Nothing -> locate it $ none
    Just Py.ExpressionList { extraChildren = [val] } -> compile val >>= locate it
    Just Py.ExpressionList { extraChildren = vals  } -> fail ("unimplemented: return statement returning " <> show (length vals) <> " values")

  compileCC r _ = compile r


instance Compile (Py.RaiseStatement Span)
instance Compile (Py.Set Span)
instance Compile (Py.SetComprehension Span)

deriving via CompileSum (Py.SimpleStatement Span) instance Compile (Py.SimpleStatement Span)

instance Compile (Py.String Span)
instance Compile (Py.Subscript Span)

instance Compile (Py.True Span) where compile it = locate it $ bool True

instance Compile (Py.TryStatement Span)

instance Compile (Py.Tuple Span) where
  compile it@Py.Tuple { Py.extraChildren = [] } = locate it $ Core.unit
  compile it                                    = fail ("Unimplemented: non-empty tuple " <> show it)

instance Compile (Py.UnaryOperator Span)
instance Compile (Py.WhileStatement Span)
instance Compile (Py.WithStatement Span)
instance Compile (Py.Yield Span)

class GCompileSum f where
  gcompileSum :: ( CoreSyntax syn t
                 , Member (Reader SourcePath) sig
                 , Member (Reader Bindings) sig
                 , Carrier sig m
                 , MonadFail m
                 ) => f a -> m (t Name)

  gcompileCCSum :: ( CoreSyntax syn t
                   , Member (Reader SourcePath) sig
                   , Member (Reader Bindings) sig
                   , Carrier sig m
                   , MonadFail m
                   ) => f a -> m (t Name) -> m (t Name)

instance GCompileSum f => GCompileSum (M1 D d f) where
  gcompileSum (M1 f) = gcompileSum f
  gcompileCCSum (M1 f) = gcompileCCSum f

instance (GCompileSum l, GCompileSum r) => GCompileSum (l :+: r) where
  gcompileSum (L1 l) = gcompileSum l
  gcompileSum (R1 r) = gcompileSum r

  gcompileCCSum (L1 l) = gcompileCCSum l
  gcompileCCSum (R1 r) = gcompileCCSum r

instance Compile t => GCompileSum (M1 C c (M1 S s (K1 R t))) where
  gcompileSum (M1 (M1 (K1 t))) = compile t
  gcompileCCSum (M1 (M1 (K1 t))) = compileCC t

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.Python.Tags
  ( ToTags (..),
  )
where

import AST.Element
import qualified AST.Parse as Parse
import AST.Token
import AST.Traversable1
import Control.Effect.Reader
import Control.Effect.Writer
import Control.Effect.State
import Data.Foldable
import Data.Text as Text
import qualified Language.Python.AST as Py
import Proto.Semantic as P
import Source.Loc
import Source.Range
import Source.Source as Source
import qualified Tags.Tagging.Precise as Tags

class ToTags t where
  tags ::
    ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
      Has (Writer Tags.Tags) sig m
    ) =>
    t Loc ->
    m ()
  default tags ::
    ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
      Has (Writer Tags.Tags) sig m,
      Traversable1 ToTags t
    ) =>
    t Loc ->
    m ()
  tags = gtags

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

instance ToTags (Token sym n) where tags _ = pure ()

keywordFunctionCall ::
  ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
    Has (Writer Tags.Tags) sig m,
    Traversable1 ToTags t
  ) =>
  t Loc ->
  Loc ->
  Range ->
  Text ->
  m ()
keywordFunctionCall t loc range name = Tags.yield name P.FUNCTION P.DEFINITION loc range >> gtags t

instance ToTags Py.String where
  tags Py.String {extraChildren} = for_ extraChildren $ \x -> case x of
    Parse.Success (Prj t@Py.Interpolation {}) -> tags t
    _ -> pure ()

instance ToTags Py.Interpolation where
  tags Py.Interpolation {extraChildren} = for_ extraChildren $ \x -> case x of
    Parse.Success (Prj (Py.Expression expr)) -> tags expr
    _ -> pure ()

instance ToTags Py.AssertStatement where
  tags t@Py.AssertStatement {ann = loc@Loc {byteRange}} = keywordFunctionCall t loc byteRange "assert"

instance ToTags Py.Await where
  tags t@Py.Await {ann = loc@Loc {byteRange}} = keywordFunctionCall t loc byteRange "await"

instance ToTags Py.DeleteStatement where
  tags t@Py.DeleteStatement {ann = loc@Loc {byteRange}} = keywordFunctionCall t loc byteRange "del"

instance ToTags Py.ExecStatement where
  tags t@Py.ExecStatement {ann = loc@Loc {byteRange}} = keywordFunctionCall t loc byteRange "exec"

instance ToTags Py.GlobalStatement where
  tags t@Py.GlobalStatement {ann = loc@Loc {byteRange}} = keywordFunctionCall t loc byteRange "global"

instance ToTags Py.NonlocalStatement where
  tags t@Py.NonlocalStatement {ann = loc@Loc {byteRange}} = keywordFunctionCall t loc byteRange "nonlocal"

instance ToTags Py.PrintStatement where
  tags t@Py.PrintStatement {ann = loc@Loc {byteRange}} = keywordFunctionCall t loc byteRange "print"

instance ToTags Py.FunctionDefinition where
  tags
    t@Py.FunctionDefinition
      { ann = Loc {byteRange = Range {start}},
        name = Parse.Success (Py.Identifier {text, ann}),
        body = Parse.Success (Py.Block {ann = Loc Range {start = end} _})
      } = do
      Tags.yield text P.FUNCTION P.DEFINITION ann (Range start end) >> gtags t
  tags _ = pure ()

instance ToTags Py.ClassDefinition where
  tags
    t@Py.ClassDefinition
      { ann = Loc {byteRange = Range {start}},
        name = Parse.Success (Py.Identifier {text, ann}),
        body = Parse.Success (Py.Block {ann = Loc Range {start = end} _})
      } = do
      Tags.yield text P.CLASS P.DEFINITION ann (Range start end) >> gtags t
  tags _ = pure ()

instance ToTags Py.Call where
  tags
    t@Py.Call
      { ann = Loc {byteRange},
        function = Parse.Success (Py.PrimaryExpression expr)
      } = match expr
      where
        match expr = case expr of
          Prj Py.Attribute {attribute = Parse.Success (Py.Identifier {text, ann})} -> yield text ann
          Prj Py.Identifier {text, ann} -> yield text ann
          Prj Py.Call {function = Parse.Success (Py.PrimaryExpression expr')} -> match expr' -- Nested call expression like this in Python represent creating an instance of a class and calling it: e.g. AClass()()
          Prj (Py.ParenthesizedExpression _ (Parse.Success (Prj (Py.Expression (Prj (Py.PrimaryExpression expr')))))) -> match expr' -- Parenthesized expressions
          _ -> gtags t
        yield name loc = Tags.yield name P.CALL P.REFERENCE loc byteRange >> gtags t
  tags _ = pure ()

gtags ::
  ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
    Has (Writer Tags.Tags) sig m,
    Traversable1 ToTags t
  ) =>
  t Loc ->
  m ()
gtags = traverse1_ @ToTags (const (pure ())) tags


instance ToTags Py.AliasedImport
instance ToTags Py.ArgumentList
-- instance ToTags Py.AssertStatement
instance ToTags Py.Assignment
instance ToTags Py.Attribute
instance ToTags Py.AugmentedAssignment
-- instance ToTags Py.Await
instance ToTags Py.BinaryOperator
instance ToTags Py.Block
instance ToTags Py.BooleanOperator
instance ToTags Py.BreakStatement
-- instance ToTags Py.Call
instance ToTags Py.Chevron
-- instance ToTags Py.ClassDefinition
instance ToTags Py.ComparisonOperator
instance ToTags Py.CompoundStatement
instance ToTags Py.ConcatenatedString
instance ToTags Py.ConditionalExpression
instance ToTags Py.ContinueStatement
instance ToTags Py.DecoratedDefinition
instance ToTags Py.Decorator
instance ToTags Py.DefaultParameter
-- instance ToTags Py.DeleteStatement
instance ToTags Py.Dictionary
instance ToTags Py.DictionaryComprehension
instance ToTags Py.DictionarySplat
instance ToTags Py.DottedName
instance ToTags Py.ElifClause
instance ToTags Py.Ellipsis
instance ToTags Py.ElseClause
instance ToTags Py.EscapeSequence
instance ToTags Py.ExceptClause
-- instance ToTags Py.ExecStatement
instance ToTags Py.Expression
instance ToTags Py.ExpressionList
instance ToTags Py.ExpressionStatement
instance ToTags Py.False
instance ToTags Py.FinallyClause
instance ToTags Py.Float
instance ToTags Py.ForInClause
instance ToTags Py.ForStatement
instance ToTags Py.FormatExpression
instance ToTags Py.FormatSpecifier
-- instance ToTags Py.FunctionDefinition
instance ToTags Py.FutureImportStatement
instance ToTags Py.GeneratorExpression
-- instance ToTags Py.GlobalStatement
instance ToTags Py.Identifier
instance ToTags Py.IfClause
instance ToTags Py.IfStatement
instance ToTags Py.ImportFromStatement
instance ToTags Py.ImportPrefix
instance ToTags Py.ImportStatement
instance ToTags Py.Integer
-- instance ToTags Py.Interpolation
instance ToTags Py.KeywordArgument
instance ToTags Py.Lambda
instance ToTags Py.LambdaParameters
instance ToTags Py.List
instance ToTags Py.ListComprehension
instance ToTags Py.ListSplat
instance ToTags Py.Module
instance ToTags Py.NamedExpression
instance ToTags Py.None
-- instance ToTags Py.NonlocalStatement
instance ToTags Py.NotOperator
instance ToTags Py.Pair
instance ToTags Py.Parameter
instance ToTags Py.Parameters
instance ToTags Py.ParenthesizedExpression
instance ToTags Py.PassStatement
instance ToTags Py.PrimaryExpression
-- instance ToTags Py.PrintStatement
instance ToTags Py.RaiseStatement
instance ToTags Py.RelativeImport
instance ToTags Py.ReturnStatement
instance ToTags Py.Set
instance ToTags Py.SetComprehension
instance ToTags Py.SimpleStatement
instance ToTags Py.Slice
instance ToTags Py.Subscript
instance ToTags Py.True
instance ToTags Py.TryStatement
instance ToTags Py.Tuple
instance ToTags Py.Type
instance ToTags Py.TypeConversion
instance ToTags Py.TypedDefaultParameter
instance ToTags Py.TypedParameter
instance ToTags Py.UnaryOperator
instance ToTags Py.Variables
instance ToTags Py.WhileStatement
instance ToTags Py.WildcardImport
instance ToTags Py.WithItem
instance ToTags Py.WithStatement
instance ToTags Py.Yield

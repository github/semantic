{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.TypeScript.Tags
  ( ToTags (..),
  )
where

import AST.Element
import qualified AST.Parse as Parse
import AST.Token
import AST.Traversable1
import Control.Effect.Reader
import Control.Effect.Writer
import Data.Foldable
import Data.Text as Text
import qualified Language.TypeScript.AST as Ts
import Source.Loc
import Source.Source as Source
import Tags.Tag
import qualified Tags.Tagging.Precise as Tags

class ToTags t where
  tags ::
    ( Has (Reader Source) sig m,
      Has (Writer Tags.Tags) sig m
    ) =>
    t Loc ->
    m ()
  default tags ::
    ( Has (Reader Source) sig m,
      Has (Writer Tags.Tags) sig m,
      Traversable1 ToTags t
    ) =>
    t Loc ->
    m ()
  tags = gtags

instance ToTags (Ts.Function Parse.Err) where
  tags t@Ts.Function {ann = Loc {byteRange}, name = Just (Parse.Succeed Ts.Identifier {text, ann})} =
    yieldTag text Function ann byteRange >> gtags t
  tags t = gtags t

instance ToTags (Ts.FunctionSignature Parse.Err) where
  tags t@Ts.FunctionSignature {ann = Loc {byteRange}, name = Parse.Succeed Ts.Identifier {text, ann}} =
    yieldTag text Function ann byteRange >> gtags t

instance ToTags (Ts.FunctionDeclaration Parse.Err) where
  tags t@Ts.FunctionDeclaration {ann = Loc {byteRange}, name = Parse.Succeed Ts.Identifier {text, ann}} =
    yieldTag text Function ann byteRange >> gtags t

instance ToTags (Ts.MethodDefinition Parse.Err) where
  tags t@Ts.MethodDefinition {ann = Loc {byteRange}, name} = case name of
    Parse.Succeed (Prj Ts.PropertyIdentifier {text, ann}) -> yieldTag text Method ann byteRange >> gtags t
    _ -> gtags t

instance ToTags (Ts.Pair Parse.Err) where
  tags t@Ts.Pair {ann = Loc {byteRange}, key, value = Parse.Succeed (Ts.Expression expr)} = case (key, expr) of
    (Parse.Succeed (Prj Ts.PropertyIdentifier {text, ann}), Prj Ts.Function {}) -> yield text ann
    (Parse.Succeed (Prj Ts.PropertyIdentifier {text, ann}), Prj Ts.ArrowFunction {}) -> yield text ann
    _ -> gtags t
    where
      yield text loc = yieldTag text Function loc byteRange >> gtags t

instance ToTags (Ts.ClassDeclaration Parse.Err) where
  tags t@Ts.ClassDeclaration {ann = Loc {byteRange}, name = Parse.Succeed Ts.TypeIdentifier {text, ann}} =
    yieldTag text Class ann byteRange >> gtags t

instance ToTags (Ts.CallExpression Parse.Err) where
  tags t@Ts.CallExpression {ann = Loc {byteRange}, function = (Parse.Succeed (Ts.Expression expr))} = match expr
    where
      match expr = case expr of
        Prj Ts.Identifier {text, ann} -> yield text ann
        Prj Ts.NewExpression {constructor = Parse.Succeed (Prj Ts.Identifier {text, ann})} -> yield text ann
        Prj Ts.CallExpression {function = Parse.Succeed (Ts.Expression expr)} -> match expr
        Prj Ts.MemberExpression {property = Parse.Succeed (Ts.PropertyIdentifier {text, ann})} -> yield text ann
        -- Prj Ts.Function {name = Just Ts.Identifier {text, ann}} -> yield text ann
        -- Prj Ts.ParenthesizedExpression {extraChildren} -> for_ extraChildren $ \x -> case x of
        --   Prj (Ts.Expression expr) -> match expr
        --   _ -> tags x
        -- _ -> gtags t
      yield name loc = yieldTag name Call loc byteRange >> gtags t

instance ToTags (Ts.Class Parse.Err) where
  tags t@Ts.Class {ann = Loc {byteRange}, name = Just (Parse.Succeed (Ts.TypeIdentifier {text, ann}))} =
    yieldTag text Class ann byteRange >> gtags t
  tags t = gtags t

instance ToTags (Ts.Module Parse.Err) where
  tags t@Ts.Module {ann = Loc {byteRange}, name} = case name of
    Parse.Succeed (Prj Ts.Identifier {text, ann}) -> yieldTag text Module ann byteRange >> gtags t
    _ -> gtags t

instance ToTags (Ts.VariableDeclarator Parse.Err) where
  tags t@Ts.VariableDeclarator {ann = Loc {byteRange}, name, value = Just (Parse.Succeed (Ts.Expression expr))} =
    case (expr, name) of
      (Prj Ts.Function {}, Parse.Succeed (Prj Ts.Identifier {text, ann})) -> yield text ann
      (Prj Ts.ArrowFunction {}, Parse.Succeed (Prj Ts.Identifier {text, ann})) -> yield text ann
      _ -> gtags t
    where
      yield text loc = yieldTag text Function loc byteRange >> gtags t
  tags t = gtags t

instance ToTags (Ts.AssignmentExpression Parse.Err) where
  tags t@Ts.AssignmentExpression {ann = Loc {byteRange}, left, right = (Parse.Succeed (Ts.Expression expr))} =
    case (left, expr) of
      (Parse.Succeed (Prj Ts.Identifier {text, ann}), Prj Ts.Function {}) -> yield text ann
      (Parse.Succeed (Prj Ts.Identifier {text, ann}), Prj Ts.ArrowFunction {}) -> yield text ann
      -- (Prj Ts.MemberExpression {property = Ts.PropertyIdentifier {text, ann}}, Prj Ts.Function {}) -> yield text ann
      -- (Prj Ts.MemberExpression {property = Ts.PropertyIdentifier {text, ann}}, Prj Ts.ArrowFunction {}) -> yield text ann
      _ -> gtags t
    where
      yield text loc = yieldTag text Function loc byteRange >> gtags t

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

instance ToTags (Token sym n Parse.Err) where tags _ = pure ()

gtags ::
  ( Has (Reader Source) sig m,
    Has (Writer Tags.Tags) sig m,
    Traversable1 ToTags t
  ) =>
  t Loc ->
  m ()
gtags = traverse1_ @ToTags (const (pure ())) tags

-- These are all valid, but point to built-in functions (e.g. require) that a la
-- carte doesn't display and since we have nothing to link to yet (can't
-- jump-to-def), we hide them from the current tags output.
nameBlacklist :: [Text]
nameBlacklist = ["require"]

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> Kind -> Loc -> Range -> m ()
yieldTag name Call _ _ | name `elem` nameBlacklist = pure ()
yieldTag name kind loc srcLineRange = do
  src <- ask @Source
  Tags.yield (Tag name kind loc (Tags.firstLine src srcLineRange) Nothing)

{- ORMOLU_DISABLE -}
instance ToTags (Ts.AbstractClassDeclaration Parse.Err)
instance ToTags (Ts.AbstractMethodSignature Parse.Err)
instance ToTags (Ts.AccessibilityModifier  Parse.Err)
instance ToTags (Ts.AmbientDeclaration Parse.Err)
instance ToTags (Ts.Arguments Parse.Err)
instance ToTags (Ts.Array Parse.Err)
instance ToTags (Ts.ArrayPattern Parse.Err)
instance ToTags (Ts.ArrayType Parse.Err)
instance ToTags (Ts.ArrowFunction Parse.Err)
instance ToTags (Ts.AsExpression Parse.Err)
-- instance ToTags Ts.AssignmentExpression
instance ToTags (Ts.AssignmentPattern Parse.Err)
instance ToTags (Ts.AugmentedAssignmentExpression Parse.Err)
instance ToTags (Ts.AwaitExpression Parse.Err)
instance ToTags (Ts.BinaryExpression Parse.Err)
instance ToTags (Ts.BreakStatement Parse.Err)
-- instance ToTags Ts.CallExpression
instance ToTags (Ts.CallSignature Parse.Err)
instance ToTags (Ts.CatchClause Parse.Err)
-- instance ToTags Ts.Class
instance ToTags (Ts.ClassBody Parse.Err)
-- instance ToTags Ts.ClassDeclaration
instance ToTags (Ts.ClassHeritage Parse.Err)
instance ToTags (Ts.ComputedPropertyName Parse.Err)
instance ToTags (Ts.Constraint Parse.Err)
instance ToTags (Ts.ConstructSignature Parse.Err)
instance ToTags (Ts.ConstructorType Parse.Err)
instance ToTags (Ts.ContinueStatement Parse.Err)
instance ToTags (Ts.DebuggerStatement Parse.Err)
instance ToTags (Ts.Declaration Parse.Err)
instance ToTags (Ts.Decorator Parse.Err)
instance ToTags (Ts.DefaultType Parse.Err)
instance ToTags (Ts.DestructuringPattern Parse.Err)
instance ToTags (Ts.DoStatement Parse.Err)
instance ToTags (Ts.EmptyStatement Parse.Err)
instance ToTags (Ts.EnumAssignment Parse.Err)
instance ToTags (Ts.EnumBody Parse.Err)
instance ToTags (Ts.EnumDeclaration Parse.Err)
instance ToTags (Ts.EscapeSequence Parse.Err)
instance ToTags (Ts.ExistentialType Parse.Err)
instance ToTags (Ts.ExportClause Parse.Err)
instance ToTags (Ts.ExportSpecifier Parse.Err)
instance ToTags (Ts.ExportStatement Parse.Err)
instance ToTags (Ts.Expression Parse.Err)
instance ToTags (Ts.ExpressionStatement Parse.Err)
instance ToTags (Ts.ExtendsClause Parse.Err)
instance ToTags (Ts.False Parse.Err)
instance ToTags (Ts.FinallyClause Parse.Err)
instance ToTags (Ts.FlowMaybeType Parse.Err)
instance ToTags (Ts.ForInStatement Parse.Err)
instance ToTags (Ts.ForStatement Parse.Err)
instance ToTags (Ts.FormalParameters Parse.Err)
-- instance ToTags Ts.Function
-- instance ToTags Ts.FunctionDeclaration
-- instance ToTags Ts.FunctionSignature
instance ToTags (Ts.FunctionType Parse.Err)
instance ToTags (Ts.GeneratorFunction Parse.Err)
instance ToTags (Ts.GeneratorFunctionDeclaration Parse.Err)
instance ToTags (Ts.GenericType Parse.Err)
instance ToTags (Ts.HashBangLine Parse.Err)
instance ToTags (Ts.Identifier Parse.Err)
instance ToTags (Ts.IfStatement Parse.Err)
instance ToTags (Ts.ImplementsClause Parse.Err)
instance ToTags (Ts.Import Parse.Err)
instance ToTags (Ts.ImportAlias Parse.Err)
instance ToTags (Ts.ImportClause Parse.Err)
instance ToTags (Ts.ImportRequireClause Parse.Err)
instance ToTags (Ts.ImportSpecifier Parse.Err)
instance ToTags (Ts.ImportStatement Parse.Err)
instance ToTags (Ts.IndexSignature Parse.Err)
instance ToTags (Ts.IndexTypeQuery Parse.Err)
instance ToTags (Ts.InterfaceDeclaration Parse.Err)
instance ToTags (Ts.InternalModule Parse.Err)
instance ToTags (Ts.IntersectionType Parse.Err)
instance ToTags (Ts.JsxAttribute Parse.Err)
instance ToTags (Ts.JsxClosingElement Parse.Err)
instance ToTags (Ts.JsxElement Parse.Err)
instance ToTags (Ts.JsxExpression Parse.Err)
instance ToTags (Ts.JsxFragment Parse.Err)
instance ToTags (Ts.JsxNamespaceName Parse.Err)
instance ToTags (Ts.JsxOpeningElement Parse.Err)
instance ToTags (Ts.JsxSelfClosingElement Parse.Err)
instance ToTags (Ts.JsxText Parse.Err)
instance ToTags (Ts.LabeledStatement Parse.Err)
instance ToTags (Ts.LexicalDeclaration Parse.Err)
instance ToTags (Ts.LiteralType Parse.Err)
instance ToTags (Ts.LookupType Parse.Err)
instance ToTags (Ts.MappedTypeClause Parse.Err)
instance ToTags (Ts.MemberExpression Parse.Err)
instance ToTags (Ts.MetaProperty Parse.Err)
-- instance ToTags Ts.MethodDefinition
instance ToTags (Ts.MethodSignature Parse.Err)
-- instance ToTags Ts.Module
instance ToTags (Ts.NamedImports Parse.Err)
instance ToTags (Ts.NamespaceImport Parse.Err)
instance ToTags (Ts.NestedIdentifier Parse.Err)
instance ToTags (Ts.NestedTypeIdentifier Parse.Err)
instance ToTags (Ts.NewExpression Parse.Err)
instance ToTags (Ts.NonNullExpression Parse.Err)
instance ToTags (Ts.Null Parse.Err)
instance ToTags (Ts.Number Parse.Err)
instance ToTags (Ts.Object Parse.Err)
instance ToTags (Ts.ObjectPattern Parse.Err)
instance ToTags (Ts.ObjectType Parse.Err)
instance ToTags (Ts.OptionalParameter Parse.Err)
-- instance ToTags Ts.Pair
instance ToTags (Ts.ParenthesizedExpression Parse.Err)
instance ToTags (Ts.ParenthesizedType Parse.Err)
instance ToTags (Ts.PredefinedType Parse.Err)
instance ToTags (Ts.Program Parse.Err)
instance ToTags (Ts.PropertyIdentifier Parse.Err)
instance ToTags (Ts.PropertySignature Parse.Err)
instance ToTags (Ts.PublicFieldDefinition Parse.Err)
instance ToTags (Ts.Readonly Parse.Err)
instance ToTags (Ts.Regex Parse.Err)
instance ToTags (Ts.RegexFlags Parse.Err)
instance ToTags (Ts.RegexPattern Parse.Err)
instance ToTags (Ts.RequiredParameter Parse.Err)
instance ToTags (Ts.RestParameter Parse.Err)
instance ToTags (Ts.ReturnStatement Parse.Err)
instance ToTags (Ts.SequenceExpression Parse.Err)
instance ToTags (Ts.ShorthandPropertyIdentifier Parse.Err)
instance ToTags (Ts.SpreadElement Parse.Err)
instance ToTags (Ts.Statement Parse.Err)
instance ToTags (Ts.StatementBlock Parse.Err)
instance ToTags (Ts.StatementIdentifier Parse.Err)
instance ToTags (Ts.String Parse.Err)
instance ToTags (Ts.SubscriptExpression Parse.Err)
instance ToTags (Ts.Super Parse.Err)
instance ToTags (Ts.SwitchBody Parse.Err)
instance ToTags (Ts.SwitchCase Parse.Err)
instance ToTags (Ts.SwitchDefault Parse.Err)
instance ToTags (Ts.SwitchStatement Parse.Err)
instance ToTags (Ts.TemplateString Parse.Err)
instance ToTags (Ts.TemplateSubstitution Parse.Err)
instance ToTags (Ts.TernaryExpression Parse.Err)
instance ToTags (Ts.This Parse.Err)
instance ToTags (Ts.ThrowStatement Parse.Err)
instance ToTags (Ts.True Parse.Err)
instance ToTags (Ts.TryStatement Parse.Err)
instance ToTags (Ts.TupleType Parse.Err)
instance ToTags (Ts.TypeAliasDeclaration Parse.Err)
instance ToTags (Ts.TypeAnnotation Parse.Err)
instance ToTags (Ts.TypeArguments Parse.Err)
instance ToTags (Ts.TypeAssertion Parse.Err)
instance ToTags (Ts.TypeIdentifier Parse.Err)
instance ToTags (Ts.TypeParameter Parse.Err)
instance ToTags (Ts.TypeParameters Parse.Err)
instance ToTags (Ts.TypePredicate Parse.Err)
instance ToTags (Ts.TypeQuery Parse.Err)
instance ToTags (Ts.UnaryExpression Parse.Err)
instance ToTags (Ts.Undefined Parse.Err)
instance ToTags (Ts.UnionType Parse.Err)
instance ToTags (Ts.UpdateExpression Parse.Err)
instance ToTags (Ts.VariableDeclaration Parse.Err)
-- instance ToTags Ts.VariableDeclarator
instance ToTags (Ts.WhileStatement Parse.Err)
instance ToTags (Ts.WithStatement Parse.Err)
instance ToTags (Ts.YieldExpression Parse.Err)
{- ORMOLU_ENABLE -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.TypeScript.Tags
  ( ToTags (..),
  )
where

import           AST.Element
import qualified AST.Parse as Parse
import           AST.Token
import           AST.Traversable1
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Effect.Writer
import           Data.Foldable
import           Data.Text (Text)
import qualified Language.TypeScript.AST as Ts
import           Proto.Semantic as P
import           Source.Loc
import           Source.Source as Source
import qualified Tags.Tagging.Precise as Tags

class ToTags (t :: * -> *) where
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

instance ToTags Ts.Function where
  tags t@Ts.Function {ann = Loc {byteRange}, name = Just (Parse.Success Ts.Identifier {text, ann})} =
    yieldTag text P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags t = gtags t

instance ToTags Ts.FunctionSignature where
  tags t@Ts.FunctionSignature {ann = Loc {byteRange}, name = Parse.Success (Ts.Identifier {text, ann})} =
    yieldTag text P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Ts.FunctionDeclaration where
  tags t@Ts.FunctionDeclaration {ann = Loc {byteRange}, name = Parse.Success (Ts.Identifier {text, ann})} =
    yieldTag text P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags Ts.MethodDefinition where
  tags t@Ts.MethodDefinition {ann = Loc {byteRange}, name} = case name of
    Parse.Success (Prj Ts.PropertyIdentifier {text, ann}) -> yieldTag text P.METHOD P.DEFINITION ann byteRange >> gtags t
    _ -> gtags t

instance ToTags Ts.Pair where
  tags t@Ts.Pair {ann = Loc {byteRange}, key = Parse.Success key, value = Parse.Success (Ts.Expression expr)} = case (key, expr) of
    (Prj Ts.PropertyIdentifier {text, ann}, Prj Ts.Function {})      -> yield text ann
    (Prj Ts.PropertyIdentifier {text, ann}, Prj Ts.ArrowFunction {}) -> yield text ann
    _                                                                -> gtags t
    where
      yield text loc = yieldTag text P.FUNCTION P.DEFINITION loc byteRange >> gtags t
  tags _ = pure ()

instance ToTags Ts.ClassDeclaration where
  tags t@Ts.ClassDeclaration {ann = Loc {byteRange}, name = Parse.Success (Ts.TypeIdentifier {text, ann})} =
    yieldTag text P.CLASS P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags Ts.CallExpression where
  tags t@Ts.CallExpression {ann = Loc {byteRange}, function = Parse.Success (Ts.Expression expr)} = match expr
    where
      match expr = case expr of
        Prj Ts.Identifier {text, ann} -> yield text ann
        Prj Ts.NewExpression {constructor = EPrj Ts.Identifier {text, ann}} -> yield text ann
        Prj Ts.CallExpression {function = Parse.Success (Ts.Expression expr)} -> match expr
        Prj Ts.MemberExpression {property = Parse.Success (Ts.PropertyIdentifier {text, ann})} -> yield text ann
        Prj Ts.Function {name = Just (Parse.Success (Ts.Identifier {text, ann}))} -> yield text ann
        Prj Ts.ParenthesizedExpression {extraChildren} -> for_ extraChildren $ \x -> case x of
          EPrj (Ts.Expression expr) -> match expr
          Parse.Success x           -> tags x
          Parse.Fail _              -> pure ()
        _ -> gtags t
      yield name loc = yieldTag name P.CALL P.REFERENCE loc byteRange >> gtags t
  tags _ = pure ()

instance ToTags Ts.Class where
  tags t@Ts.Class {ann = Loc {byteRange}, name = Just (Parse.Success Ts.TypeIdentifier {text, ann})} =
    yieldTag text P.CLASS P.DEFINITION ann byteRange >> gtags t
  tags t = gtags t

instance ToTags Ts.Module where
  tags t@Ts.Module {ann = Loc {byteRange}, name} = case name of
    Parse.Success (Prj Ts.Identifier {text, ann}) -> yieldTag text P.MODULE P.DEFINITION ann byteRange >> gtags t
    _                                             -> gtags t

instance ToTags Ts.VariableDeclarator where
  tags t@Ts.VariableDeclarator {ann = Loc {byteRange}, name, value = Just (Parse.Success (Ts.Expression expr))} =
    case (expr, name) of
      (Prj Ts.Function {}, Parse.Success (Prj Ts.Identifier {text, ann}))      -> yield text ann
      (Prj Ts.ArrowFunction {}, Parse.Success (Prj Ts.Identifier {text, ann})) -> yield text ann
      _                                                                        -> gtags t
    where
      yield text loc = yieldTag text P.FUNCTION P.DEFINITION loc byteRange >> gtags t
  tags t = gtags t

instance ToTags Ts.AssignmentExpression where
  tags t@Ts.AssignmentExpression {ann = Loc {byteRange}, left, right = Parse.Success (Ts.Expression expr)} =
    case (left, expr) of
      (Parse.Success (Prj Ts.Identifier {text, ann}), Prj Ts.Function {}) -> yield text ann
      (Parse.Success (Prj Ts.Identifier {text, ann}), Prj Ts.ArrowFunction {}) -> yield text ann
      (EPrj Ts.MemberExpression {property = Parse.Success (Ts.PropertyIdentifier {text, ann})}, Prj Ts.Function {}) -> yield text ann
      (EPrj Ts.MemberExpression {property = Parse.Success (Ts.PropertyIdentifier {text, ann})}, Prj Ts.ArrowFunction {}) -> yield text ann
      _ -> gtags t
    where
      yield text loc = yieldTag text P.FUNCTION P.DEFINITION loc byteRange >> gtags t
  tags _ = pure ()

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

instance ToTags (Token sym n) where tags _ = pure ()

gtags ::
  ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
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

yieldTag :: (Has (Reader Source) sig m, Has (State Tags.LineIndices) sig m, Has (Writer Tags.Tags) sig m) => Text -> P.SyntaxType -> P.NodeType -> Loc -> Range -> m ()
yieldTag name P.CALL _ _ _ | name `elem` nameBlacklist = pure ()
yieldTag name kind ty loc srcLineRange = Tags.yield name kind ty loc srcLineRange

{- ORMOLU_DISABLE -}
instance ToTags Ts.AbstractClassDeclaration
instance ToTags Ts.AbstractMethodSignature
instance ToTags Ts.AccessibilityModifier
instance ToTags Ts.AmbientDeclaration
instance ToTags Ts.Arguments
instance ToTags Ts.Array
instance ToTags Ts.ArrayPattern
instance ToTags Ts.ArrayType
instance ToTags Ts.ArrowFunction
instance ToTags Ts.AsExpression
-- instance ToTags Ts.AssignmentExpression
instance ToTags Ts.AssignmentPattern
instance ToTags Ts.AugmentedAssignmentExpression
instance ToTags Ts.AwaitExpression
instance ToTags Ts.BinaryExpression
instance ToTags Ts.BreakStatement
-- instance ToTags Ts.CallExpression
instance ToTags Ts.CallSignature
instance ToTags Ts.CatchClause
-- instance ToTags Ts.Class
instance ToTags Ts.ClassBody
-- instance ToTags Ts.ClassDeclaration
instance ToTags Ts.ClassHeritage
instance ToTags Ts.ComputedPropertyName
instance ToTags Ts.Constraint
instance ToTags Ts.ConstructSignature
instance ToTags Ts.ConstructorType
instance ToTags Ts.ContinueStatement
instance ToTags Ts.DebuggerStatement
instance ToTags Ts.Declaration
instance ToTags Ts.Decorator
instance ToTags Ts.DefaultType
instance ToTags Ts.DestructuringPattern
instance ToTags Ts.DoStatement
instance ToTags Ts.EmptyStatement
instance ToTags Ts.EnumAssignment
instance ToTags Ts.EnumBody
instance ToTags Ts.EnumDeclaration
instance ToTags Ts.EscapeSequence
instance ToTags Ts.ExistentialType
instance ToTags Ts.ExportClause
instance ToTags Ts.ExportSpecifier
instance ToTags Ts.ExportStatement
instance ToTags Ts.Expression
instance ToTags Ts.ExpressionStatement
instance ToTags Ts.ExtendsClause
instance ToTags Ts.False
instance ToTags Ts.FinallyClause
instance ToTags Ts.FlowMaybeType
instance ToTags Ts.ForInStatement
instance ToTags Ts.ForStatement
instance ToTags Ts.FormalParameters
-- instance ToTags Ts.Function
-- instance ToTags Ts.FunctionDeclaration
-- instance ToTags Ts.FunctionSignature
instance ToTags Ts.FunctionType
instance ToTags Ts.GeneratorFunction
instance ToTags Ts.GeneratorFunctionDeclaration
instance ToTags Ts.GenericType
instance ToTags Ts.HashBangLine
instance ToTags Ts.Identifier
instance ToTags Ts.IfStatement
instance ToTags Ts.ImplementsClause
instance ToTags Ts.Import
instance ToTags Ts.ImportAlias
instance ToTags Ts.ImportClause
instance ToTags Ts.ImportRequireClause
instance ToTags Ts.ImportSpecifier
instance ToTags Ts.ImportStatement
instance ToTags Ts.IndexSignature
instance ToTags Ts.IndexTypeQuery
instance ToTags Ts.InterfaceDeclaration
instance ToTags Ts.InternalModule
instance ToTags Ts.IntersectionType
instance ToTags Ts.JsxAttribute
instance ToTags Ts.JsxClosingElement
instance ToTags Ts.JsxElement
instance ToTags Ts.JsxExpression
instance ToTags Ts.JsxFragment
instance ToTags Ts.JsxNamespaceName
instance ToTags Ts.JsxOpeningElement
instance ToTags Ts.JsxSelfClosingElement
instance ToTags Ts.JsxText
instance ToTags Ts.LabeledStatement
instance ToTags Ts.LexicalDeclaration
instance ToTags Ts.LiteralType
instance ToTags Ts.LookupType
instance ToTags Ts.MappedTypeClause
instance ToTags Ts.MemberExpression
instance ToTags Ts.MetaProperty
-- instance ToTags Ts.MethodDefinition
instance ToTags Ts.MethodSignature
-- instance ToTags Ts.Module
instance ToTags Ts.NamedImports
instance ToTags Ts.NamespaceImport
instance ToTags Ts.NestedIdentifier
instance ToTags Ts.NestedTypeIdentifier
instance ToTags Ts.NewExpression
instance ToTags Ts.NonNullExpression
instance ToTags Ts.Null
instance ToTags Ts.Number
instance ToTags Ts.Object
instance ToTags Ts.ObjectPattern
instance ToTags Ts.ObjectType
instance ToTags Ts.OptionalParameter
-- instance ToTags Ts.Pair
instance ToTags Ts.ParenthesizedExpression
instance ToTags Ts.ParenthesizedType
instance ToTags Ts.PredefinedType
instance ToTags Ts.Program
instance ToTags Ts.PropertyIdentifier
instance ToTags Ts.PropertySignature
instance ToTags Ts.PublicFieldDefinition
instance ToTags Ts.Readonly
instance ToTags Ts.Regex
instance ToTags Ts.RegexFlags
instance ToTags Ts.RegexPattern
instance ToTags Ts.RequiredParameter
instance ToTags Ts.RestParameter
instance ToTags Ts.ReturnStatement
instance ToTags Ts.SequenceExpression
instance ToTags Ts.ShorthandPropertyIdentifier
instance ToTags Ts.SpreadElement
instance ToTags Ts.Statement
instance ToTags Ts.StatementBlock
instance ToTags Ts.StatementIdentifier
instance ToTags Ts.String
instance ToTags Ts.SubscriptExpression
instance ToTags Ts.Super
instance ToTags Ts.SwitchBody
instance ToTags Ts.SwitchCase
instance ToTags Ts.SwitchDefault
instance ToTags Ts.SwitchStatement
instance ToTags Ts.TemplateString
instance ToTags Ts.TemplateSubstitution
instance ToTags Ts.TernaryExpression
instance ToTags Ts.This
instance ToTags Ts.ThrowStatement
instance ToTags Ts.True
instance ToTags Ts.TryStatement
instance ToTags Ts.TupleType
instance ToTags Ts.TypeAliasDeclaration
instance ToTags Ts.TypeAssertion
instance ToTags Ts.TypeAnnotation
instance ToTags Ts.TypeArguments
instance ToTags Ts.TypeIdentifier
instance ToTags Ts.TypeParameter
instance ToTags Ts.TypeParameters
instance ToTags Ts.TypePredicate
instance ToTags Ts.TypeQuery
instance ToTags Ts.UnaryExpression
instance ToTags Ts.Undefined
instance ToTags Ts.UnionType
instance ToTags Ts.UpdateExpression
instance ToTags Ts.VariableDeclaration
-- instance ToTags Ts.VariableDeclarator
instance ToTags Ts.WhileStatement
instance ToTags Ts.WithStatement
instance ToTags Ts.YieldExpression
{- ORMOLU_ENABLE -}

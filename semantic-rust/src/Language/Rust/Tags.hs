{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Language.Rust.Tags
( ToTags(..)
) where

import           AST.Element
import qualified AST.Parse as Parse
import           AST.Token
import           AST.Traversable1
import qualified AST.Unmarshal as TS
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Effect.Writer
import           Data.Text as Text
import qualified Language.Rust.AST as Rust
import           Source.Loc
import           Source.Source as Source
import           Tags.Tag()
import qualified Tags.Tagging.Precise as Tags
import           Proto.Semantic as P


class ToTags t where
  tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       )
    => t Loc
    -> m ()
  default tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       , Traversable1 ToTags t
       )
    => t Loc
    -> m ()
  tags = gtags

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

instance ToTags (Token sym n) where tags _ = pure ()

gtags
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Traversable1 ToTags t
     )
  => t Loc
  -> m ()
gtags = traverse1_ @ToTags (const (pure ())) tags

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> P.SyntaxType -> P.NodeType -> Loc -> Range -> m ()
yieldTag name kind ty loc srcLineRange = do
  src <- ask @Source
  Tags.yield (Tag name kind ty loc (Tags.firstLine src srcLineRange) Nothing)


instance ToTags Rust.SourceFile where
  tags
    t@Rust.SourceFile
      { extraChildren,
        ann = loc@Loc {byteRange}
      } = case extraChildren of
      EPrj (Rust.DeclarationStatement {text, ann}) -> yield text ann
      EPrj (Rust.Expression {text, ann}) -> yield text ann
        where
          yield name ann = yieldTag name P.MODULE P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.AssignmentExpression where
  tags
    t@Rust.AssignmentExpression
      { ann = loc@Loc {byteRange},
        left = Parse.Success (Rust.Expression {text, ann})
      } = yieldTag text P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.Block where
  tags
    t@Rust.Block
      { extraChildren,
        ann = loc@Loc {byteRange}
      } = case extraChildren of
      EPrj Rust.DeclarationStatement -> yield text ann
      EPrj Rust.Expression -> yield text ann
        where
          yield name ann = yieldTag name P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.CallExpression where
  tags
    t@Rust.CallExpression 
      {
        ann = loc@Loc {byteRange},
        args = Parse.Success (Rust.Arguments {text, ann}),
        expr = Parse.Success (Rust.Expression {text, ann})
      } = yieldTag expr P.FUNCTION P.DEFINITION ann byteRange >> gtags t


instance ToTags Rust.ClosureExpression where
  tags
    t@Rust.ClosureExpression
      { ann = loc@Loc {byteRange},
        body = Parse.Success (Rust.Expression {text, ann}),
        parameters = Parse.Success (Rust.ClosureParameters {text, ann}),
        returnType = Parse.Success (Rust.Type {text, ann})
      } = yieldTag body P.METHOD P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.FieldDeclaration where
  tags
    t@Rust.FieldDeclaration
      { ann = loc@Loc {byteRange},
        name = Parse.Success (Rust.FieldIdentifier {text, ann}),
        type' = Parse.Success (Rust.Type {text, ann}),
        extraChildren
      } = case extraChildren of
      Just (Parse.Success (Rust.VisibilityModifier)) -> yield text ann
      _ -> pure ()
      where
        yield name ann = yieldTag name P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.FunctionItem where 
  tags
    t@Rust.FunctionItem {
      ann = loc@Loc {byteRange},
      body = Parse.Success (Rust.Block {text, ann}),
      name = Parse.Success (Rust.Identifier {text, ann}),
      parameters = Parse.Success (Rust.Parameters {text, ann})
      returnType,
      typeParameters,
      extraChildren
    } = do
      tags 
      case returnType of
        Just (Parse.Success (Rust.Type)) -> yield text ann
        _ -> pure ()
      case typeParameters of
        Just (Parse.Success (Rust.TypeParameters)) -> yield text ann
        _ -> pure () 
      case extraChildren of 
        EPrj Rust.FunctionModifiers -> yield text ann 
        EPrj Rust.VisibilityModifier -> yield text ann
        EPrj Rust.WhereClause -> yield text ann
      where yield name ann = yieldTag name P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.FunctionModifiers where
  tags
    t@Rust.FunctionModifiers
      { ann = loc@Loc {byteRange},
        extraChildren = Parse.Success (Rust.ExternModifier {text, ann})
      } = yieldTag body P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.FunctionSignatureItem where
  tags
    t@Rust.FunctionSignatureItem
      { ann = loc@Loc {byteRange},
        name,
        parameters = Parse.Success (Rust.Parameters {text, ann}),
        returnType = Parse.Success (Rust.Type {text, ann}),
        typeParameters = Parse.Success (Rust.TypeParameters {text, ann}),
        extraChildren
      } = do
      case name of
        EPrj (Rust.Identifier) -> yield text ann
        EPrj (Rust.Metavariable) -> yield text ann
      case extraChildren of
        EPrj Rust.FunctionModifiers -> yield text ann
        EPrj Rust.VisibilityModifier -> yield text ann
        EPrj Rust.WhereClause -> yield text ann
      where
        yield name ann = yieldTag name P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.FunctionType where
  tags
    t@Rust.FunctionType
      { ann = loc@Loc {byteRange},
        parameters = Parse.Success (Rust.Parameters {text, ann}),
        returnType = Parse.Success (Rust.Type {text, ann}),
        trait,
        extraChildren
      } = do
      case trait of
        EPrj Rust.ScopedTypeIdentifier -> yield text ann
        EPrj Rust.TypeIdentifier -> yield text ann
      case extraChildren of
        EPrj Rust.ForLifetimes -> yield text ann
        EPrj Rust.ForModifiers -> yield text ann
      where
        yield name ann = yieldTag name P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.GenericFunction where
  tags
    t@Rust.GenericFunction
      { ann = loc@Loc {byteRange},
        function,
        typeArgs = Parse.Success (Rust.TypeArguments {text, ann})
      } = do
      case function of
        EPrj Rust.FieldExpression -> yield text ann
        EPrj Rust.Identifier -> yield text ann
        EPrj Rust.ScopedIdentifier -> yield text ann
      where
        yield name ann = yieldTag function P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.AbstractType
instance ToTags Rust.Arguments
instance ToTags Rust.ArrayExpression
instance ToTags Rust.ArrayType
-- instance ToTags Rust.AssignmentExpression
instance ToTags Rust.AssociatedType
instance ToTags Rust.AsyncBlock
instance ToTags Rust.AttributeItem
instance ToTags Rust.AwaitExpression
instance ToTags Rust.BaseFieldInitializer
instance ToTags Rust.BinaryExpression
-- instance ToTags Rust.Block
instance ToTags Rust.BlockComment
instance ToTags Rust.BooleanLiteral
instance ToTags Rust.BoundedType
instance ToTags Rust.BracketedType
instance ToTags Rust.BreakExpression
-- instance ToTags Rust.CallExpression
instance ToTags Rust.CapturedPattern
instance ToTags Rust.CharLiteral
-- instance ToTags Rust.ClosureExpression
instance ToTags Rust.ClosureParameters
instance ToTags Rust.CompoundAssignmentExpr
instance ToTags Rust.ConstItem
instance ToTags Rust.ConstParameter
instance ToTags Rust.ConstrainedTypeParameter
instance ToTags Rust.ContinueExpression
instance ToTags Rust.Crate
instance ToTags Rust.DeclarationList
instance ToTags Rust.DeclarationStatement
instance ToTags Rust.DynamicType
instance ToTags Rust.EmptyStatement
instance ToTags Rust.EmptyType
instance ToTags Rust.EnumItem
instance ToTags Rust.EnumVariant
instance ToTags Rust.EnumVariantList
instance ToTags Rust.EscapeSequence
instance ToTags Rust.Expression
instance ToTags Rust.ExternCrateDeclaration
instance ToTags Rust.ExternModifier
-- instance ToTags Rust.FieldDeclaration
instance ToTags Rust.FieldDeclarationList
instance ToTags Rust.FieldExpression
instance ToTags Rust.FieldIdentifier
instance ToTags Rust.FieldInitializer
instance ToTags Rust.FieldInitializerList
instance ToTags Rust.FieldPattern
instance ToTags Rust.FloatLiteral
instance ToTags Rust.ForExpression
instance ToTags Rust.ForLifetimes
instance ToTags Rust.ForeignModItem
instance ToTags Rust.FragmentSpecifier
-- instance ToTags Rust.FunctionItem
-- instance ToTags Rust.FunctionModifiers
-- instance ToTags Rust.FunctionSignatureItem
-- instance ToTags Rust.FunctionType
-- instance ToTags Rust.GenericFunction
instance ToTags Rust.GenericType
instance ToTags Rust.GenericTypeWithTurbofish
instance ToTags Rust.HigherRankedTraitBound
instance ToTags Rust.Identifier
instance ToTags Rust.IfExpression
instance ToTags Rust.IfLetExpression
instance ToTags Rust.ImplItem
instance ToTags Rust.IndexExpression
instance ToTags Rust.InnerAttributeItem
instance ToTags Rust.IntegerLiteral
instance ToTags Rust.LetDeclaration
instance ToTags Rust.Lifetime
instance ToTags Rust.LineComment
instance ToTags Rust.Literal
instance ToTags Rust.LiteralPattern
instance ToTags Rust.LoopExpression
instance ToTags Rust.LoopLabel
instance ToTags Rust.MacroDefinition
instance ToTags Rust.MacroInvocation
instance ToTags Rust.MacroRule
instance ToTags Rust.MatchArm
instance ToTags Rust.MatchBlock
instance ToTags Rust.MatchExpression
instance ToTags Rust.MatchPattern
instance ToTags Rust.MetaArguments
instance ToTags Rust.MetaItem
instance ToTags Rust.Metavariable
instance ToTags Rust.ModItem
instance ToTags Rust.MutPattern
instance ToTags Rust.MutableSpecifier
instance ToTags Rust.NegativeLiteral
instance ToTags Rust.OptionalTypeParameter
instance ToTags Rust.OrderedFieldDeclarationList
instance ToTags Rust.Parameter
instance ToTags Rust.Parameters
instance ToTags Rust.ParenthesizedExpression
instance ToTags Rust.Pattern
instance ToTags Rust.PointerType
instance ToTags Rust.PrimitiveType
instance ToTags Rust.QualifiedType
instance ToTags Rust.RangeExpression
instance ToTags Rust.RangePattern
instance ToTags Rust.RawStringLiteral
instance ToTags Rust.RefPattern
instance ToTags Rust.ReferenceExpression
instance ToTags Rust.ReferencePattern
instance ToTags Rust.ReferenceType
instance ToTags Rust.RemainingFieldPattern
instance ToTags Rust.RemovedTraitBound
instance ToTags Rust.ReturnExpression
instance ToTags Rust.ScopedIdentifier
instance ToTags Rust.ScopedTypeIdentifier
instance ToTags Rust.ScopedUseList
instance ToTags Rust.Self
instance ToTags Rust.SelfParameter
instance ToTags Rust.ShorthandFieldIdentifier
instance ToTags Rust.ShorthandFieldInitializer
instance ToTags Rust.SlicePattern
-- instance ToTags Rust.SourceFile
instance ToTags Rust.StaticItem
instance ToTags Rust.StringLiteral
instance ToTags Rust.StructExpression
instance ToTags Rust.StructItem
instance ToTags Rust.StructPattern
instance ToTags Rust.Super
instance ToTags Rust.TokenBindingPattern
instance ToTags Rust.TokenRepetition
instance ToTags Rust.TokenRepetitionPattern
instance ToTags Rust.TokenTree
instance ToTags Rust.TokenTreePattern
instance ToTags Rust.TraitBounds
instance ToTags Rust.TraitItem
instance ToTags Rust.TryExpression
instance ToTags Rust.TupleExpression
instance ToTags Rust.TuplePattern
instance ToTags Rust.TupleStructPattern
instance ToTags Rust.TupleType
instance ToTags Rust.Type
instance ToTags Rust.TypeArguments
instance ToTags Rust.TypeBinding
instance ToTags Rust.TypeCastExpression
-- instance ToTags Rust.TypeIdentifier
instance ToTags Rust.TypeItem
instance ToTags Rust.TypeParameters
instance ToTags Rust.UnaryExpression
instance ToTags Rust.UnionItem
instance ToTags Rust.UnitExpression
instance ToTags Rust.UnitType
instance ToTags Rust.UnsafeBlock
instance ToTags Rust.UseAsClause
instance ToTags Rust.UseDeclaration
instance ToTags Rust.UseList
instance ToTags Rust.UseWildcard
instance ToTags Rust.VariadicParameter
instance ToTags Rust.VisibilityModifier
instance ToTags Rust.WhereClause
instance ToTags Rust.WherePredicate
instance ToTags Rust.WhileExpression
instance ToTags Rust.WhileLetExpression

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Tags.Tag
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

-- enum SyntaxType {
--   FUNCTION = 0;
--   METHOD = 1;
--   CLASS = 2;
--   MODULE = 3;
--   CALL = 4;
--   TYPE = 5;
--   INTERFACE = 6;
--   IMPLEMENTATION = 7;
-- }

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> P.SyntaxType -> P.NodeType -> Loc -> Range -> m ()
yieldTag name kind ty loc srcLineRange = do
  src <- ask @Source
  Tags.yield (Tag name kind ty loc (Tags.firstLine src srcLineRange) Nothing)


instance ToTags Rust.ModItem where 
  tags 
    t@Rust.ModItem 
      {
        ann = loc@Loc {byteRange},
        body,
        name = Parse.Success (Rust.Identifier {text, ann})
      } = do 
        case body of 
          Just (Parse.Success decls) -> gtags decls
          _ -> pure ()
        where yield name ann = yieldTag name P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.FunctionItem where
  tags
    t@Rust.FunctionItem
      { 
        ann = loc@Loc {byteRange},
        returnType,
        body = Parse.Success block,
        name,
        parameters = Parse.Success parameters,
        typeParameters
      } = do
      case returnType of
        Just (Parse.Success type') -> gtags type'
        _ -> pure ()
      case typeParameters of
        Just (Parse.Success typeParams) -> gtags typeParams
        _ -> pure ()
      case name of
        EPrj (Rust.Identifier {text, ann}) -> yield text ann
        EPrj (Rust.Metavariable {text, ann}) -> yield text ann
        _ -> pure ()
      gtags block
      where
        yield name ann = yieldTag name P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.CallExpression where
  tags
    t@Rust.CallExpression 
      {
        ann = loc@Loc {byteRange},
        function = Parse.Success expr,
        arguments = Parse.Success args
      } = gtags expr
  tags _ = pure ()

instance ToTags Rust.EnumItem where 
  tags 
    t@Rust.EnumItem
      {
        ann = loc@Loc {byteRange},
        body = Parse.Success bod,
        name = Parse.Success (Rust.TypeIdentifier {text, ann})
      } = yieldTag text P.FUNCTION P.DEFINITION ann byteRange >> gtags t


instance ToTags Rust.FieldDeclaration where
  tags
    t@Rust.FieldDeclaration
      { ann = loc@Loc {byteRange},
        name = Parse.Success (Rust.FieldIdentifier {text, ann}),
        type' = Parse.Success type''
      } = yieldTag text P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()


instance ToTags Rust.FunctionSignatureItem where
  tags
    t@Rust.FunctionSignatureItem
      { ann = loc@Loc {byteRange},
        returnType,
        name,
        parameters = Parse.Success params,
        typeParameters
      } = do
        case returnType of 
          Just (Parse.Success type') -> gtags type'
          _ -> pure ()
        case name of
          EPrj (Rust.Identifier {text, ann}) -> yield text ann
          EPrj (Rust.Metavariable {text, ann}) -> yield text ann
      where
        yield name ann = yieldTag name P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags Rust.GenericFunction where
  tags
    t@Rust.GenericFunction
      { ann = loc@Loc {byteRange},
        function,
        typeArguments = Parse.Success typeArgs
      } = do
        case function of
          EPrj (Rust.Identifier {text, ann}) -> yield text ann
          _ -> pure ()
        gtags t
        where
          yield function ann = yieldTag function P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags Rust.StaticItem where 
  tags 
    t@Rust.StaticItem
    {
      ann = loc@Loc {byteRange},
      value,
      name = Parse.Success (Rust.Identifier {text, ann}),
      type' 
    } = do 
      case value of
        Just (Parse.Success expr) -> gtags expr 
        _ -> pure ()
    where yield name ann = yieldTag name P.FUNCTION P.DEFINITION ann byteRange >> gtags t
        

instance ToTags Rust.StructItem where 
  tags 
    t@Rust.StructItem
      {
        ann = loc@Loc {byteRange},
        body, 
        name = Parse.Success (Rust.TypeIdentifier {text, ann})
      } = yieldTag text P.FUNCTION P.REFERENCE ann byteRange >> gtags t 
        
instance ToTags Rust.UnionItem where 
  tags 
    t@Rust.UnionItem
      {
        ann = loc@Loc {byteRange},
        body,
        name = Parse.Success (Rust.TypeIdentifier {text, ann})
      } = yieldTag text P.FUNCTION P.DEFINITION ann byteRange >> gtags t

instance ToTags Rust.TraitItem where 
  tags 
    t@Rust.TraitItem
      {
        ann = loc@Loc {byteRange},
        body,
        name = Parse.Success (Rust.TypeIdentifier {text, ann})
      } = yieldTag text P.FUNCTION P.DEFINITION ann byteRange >> gtags t


instance ToTags Rust.AbstractType
instance ToTags Rust.Arguments
instance ToTags Rust.ArrayExpression
instance ToTags Rust.ArrayType
instance ToTags Rust.AssignmentExpression
instance ToTags Rust.AssociatedType
instance ToTags Rust.AsyncBlock
instance ToTags Rust.AttributeItem
instance ToTags Rust.AwaitExpression
instance ToTags Rust.BaseFieldInitializer
instance ToTags Rust.BinaryExpression
instance ToTags Rust.Block
instance ToTags Rust.BlockComment
instance ToTags Rust.BooleanLiteral
instance ToTags Rust.BoundedType
instance ToTags Rust.BracketedType
instance ToTags Rust.BreakExpression
-- instance ToTags Rust.CallExpression
instance ToTags Rust.CapturedPattern
instance ToTags Rust.CharLiteral
instance ToTags Rust.ClosureExpression
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
-- instance ToTags Rust.EnumItem
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
instance ToTags Rust.FunctionModifiers
-- instance ToTags Rust.FunctionSignatureItem
instance ToTags Rust.FunctionType
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
-- instance ToTags Rust.ModItem
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
instance ToTags Rust.SourceFile
-- instance ToTags Rust.StaticItem
instance ToTags Rust.StringLiteral
instance ToTags Rust.StructExpression
-- instance ToTags Rust.StructItem
instance ToTags Rust.StructPattern
instance ToTags Rust.Super
instance ToTags Rust.TokenBindingPattern
instance ToTags Rust.TokenRepetition
instance ToTags Rust.TokenRepetitionPattern
instance ToTags Rust.TokenTree
instance ToTags Rust.TokenTreePattern
instance ToTags Rust.TraitBounds
-- instance ToTags Rust.TraitItem
instance ToTags Rust.TryExpression
instance ToTags Rust.TupleExpression
instance ToTags Rust.TuplePattern
instance ToTags Rust.TupleStructPattern
instance ToTags Rust.TupleType
instance ToTags Rust.Type
instance ToTags Rust.TypeArguments
instance ToTags Rust.TypeBinding
instance ToTags Rust.TypeCastExpression
instance ToTags Rust.TypeIdentifier --this
instance ToTags Rust.TypeItem --this
instance ToTags Rust.TypeParameters
instance ToTags Rust.UnaryExpression
-- instance ToTags Rust.UnionItem
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

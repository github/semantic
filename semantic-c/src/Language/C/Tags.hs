{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.C.Tags
  ( ToTags (..),
  )
where

import AST.Element
import AST.Token
import AST.Traversable1
import Control.Effect.Reader
import Control.Effect.Writer
import Control.Effect.State
import qualified Language.C.AST as C
import Source.Loc
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

instance ToTags (Token sym n) where tags _ = pure ()

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

gtags ::
  ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
    Has (Writer Tags.Tags) sig m,
    Traversable1 ToTags t
  ) =>
  t Loc ->
  m ()
gtags = traverse1_ @ToTags (const (pure ())) tags

instance ToTags C.AbstractArrayDeclarator
instance ToTags C.AbstractDeclarator
instance ToTags C.AbstractFunctionDeclarator
instance ToTags C.AbstractParenthesizedDeclarator
instance ToTags C.AbstractPointerDeclarator
instance ToTags C.ArgumentList
instance ToTags C.ArrayDeclarator
instance ToTags C.AssignmentExpression
instance ToTags C.AttributeSpecifier
instance ToTags C.BinaryExpression
instance ToTags C.BitfieldClause
instance ToTags C.BreakStatement
instance ToTags C.CallExpression
instance ToTags C.CaseStatement
instance ToTags C.CastExpression
instance ToTags C.CharLiteral
instance ToTags C.CommaExpression
instance ToTags C.CompoundLiteralExpression
instance ToTags C.CompoundStatement
instance ToTags C.ConcatenatedString
instance ToTags C.ConditionalExpression
instance ToTags C.ContinueStatement
instance ToTags C.Declaration
instance ToTags C.DeclarationList
instance ToTags C.Declarator
instance ToTags C.DoStatement
instance ToTags C.Enumerator
instance ToTags C.EnumeratorList
instance ToTags C.EnumSpecifier
instance ToTags C.EscapeSequence
instance ToTags C.Expression
instance ToTags C.ExpressionStatement
instance ToTags C.False
instance ToTags C.FieldDeclaration
instance ToTags C.FieldDeclarationList
instance ToTags C.FieldDeclarator
instance ToTags C.FieldDesignator
instance ToTags C.FieldExpression
instance ToTags C.FieldIdentifier
instance ToTags C.ForStatement
instance ToTags C.FunctionDeclarator
instance ToTags C.FunctionDefinition
instance ToTags C.GotoStatement
instance ToTags C.Identifier
instance ToTags C.IfStatement
instance ToTags C.InitDeclarator
instance ToTags C.InitializerList
instance ToTags C.InitializerPair
instance ToTags C.LabeledStatement
instance ToTags C.LinkageSpecification
instance ToTags C.MacroTypeSpecifier
instance ToTags C.MsBasedModifier
instance ToTags C.MsCallModifier
instance ToTags C.MsDeclspecModifier
instance ToTags C.MsPointerModifier
instance ToTags C.MsRestrictModifier
instance ToTags C.MsSignedPtrModifier
instance ToTags C.MsUnalignedPtrModifier
instance ToTags C.MsUnsignedPtrModifier
instance ToTags C.Null
instance ToTags C.NumberLiteral
instance ToTags C.ParameterDeclaration
instance ToTags C.ParameterList
instance ToTags C.ParenthesizedDeclarator
instance ToTags C.ParenthesizedExpression
instance ToTags C.PointerDeclarator
instance ToTags C.PointerExpression
instance ToTags C.PreprocArg
instance ToTags C.PreprocCall
instance ToTags C.PreprocDef
instance ToTags C.PreprocDefined
instance ToTags C.PreprocDirective
instance ToTags C.PreprocElif
instance ToTags C.PreprocElse
instance ToTags C.PreprocFunctionDef
instance ToTags C.PreprocIf
instance ToTags C.PreprocIfdef
instance ToTags C.PreprocInclude
instance ToTags C.PreprocParams
instance ToTags C.PrimitiveType
instance ToTags C.ReturnStatement
instance ToTags C.SizedTypeSpecifier
instance ToTags C.SizeofExpression
instance ToTags C.Statement
instance ToTags C.StatementIdentifier
instance ToTags C.StorageClassSpecifier
instance ToTags C.StringLiteral
instance ToTags C.StructSpecifier
instance ToTags C.SubscriptDesignator
instance ToTags C.SubscriptExpression
instance ToTags C.SwitchStatement
instance ToTags C.SystemLibString
instance ToTags C.TranslationUnit
instance ToTags C.True
instance ToTags C.TypeDeclarator
instance ToTags C.TypeDefinition
instance ToTags C.TypeDescriptor
instance ToTags C.TypeIdentifier
instance ToTags C.TypeQualifier
instance ToTags C.TypeSpecifier
instance ToTags C.UnaryExpression
instance ToTags C.UnionSpecifier
instance ToTags C.UpdateExpression
instance ToTags C.WhileStatement

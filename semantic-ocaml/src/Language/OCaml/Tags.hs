{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Language.OCaml.Tags
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
import qualified Language.OCaml.AST as OCaml
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

gtags ::
  ( Has (Reader Source) sig m,
    Has (State Tags.LineIndices) sig m,
    Has (Writer Tags.Tags) sig m,
    Traversable1 ToTags t
  ) =>
  t Loc ->
  m ()
gtags = traverse1_ @ToTags (const (pure ())) tags

instance ToTags OCaml.AbstractType
instance ToTags OCaml.AliasedType
instance ToTags OCaml.AliasPattern
instance ToTags OCaml.AndOperator
instance ToTags OCaml.ApplicationExpression
instance ToTags OCaml.ArrayExpression
instance ToTags OCaml.ArrayGetExpression
instance ToTags OCaml.ArrayPattern
instance ToTags OCaml.AssertExpression
instance ToTags OCaml.AttributeId
instance ToTags OCaml.AttributePayload
instance ToTags OCaml.BigarrayGetExpression
instance ToTags OCaml.Boolean
instance ToTags OCaml.Character
instance ToTags OCaml.ClassApplication
instance ToTags OCaml.ClassBinding
instance ToTags OCaml.ClassBodyType
instance ToTags OCaml.ClassDefinition
instance ToTags OCaml.ClassFunction
instance ToTags OCaml.ClassFunctionType
instance ToTags OCaml.ClassInitializer
instance ToTags OCaml.ClassName
instance ToTags OCaml.ClassPath
instance ToTags OCaml.ClassTypeBinding
instance ToTags OCaml.ClassTypeDefinition
instance ToTags OCaml.ClassTypePath
instance ToTags OCaml.CoercionExpression
instance ToTags OCaml.CompilationUnit
instance ToTags OCaml.ConsExpression
instance ToTags OCaml.ConsPattern
instance ToTags OCaml.ConstrainModule
instance ToTags OCaml.ConstrainType
instance ToTags OCaml.ConstructedType
instance ToTags OCaml.ConstructorDeclaration
instance ToTags OCaml.ConstructorName
instance ToTags OCaml.ConstructorPath
instance ToTags OCaml.ConstructorPattern
instance ToTags OCaml.ConversionSpecification
instance ToTags OCaml.Directive
instance ToTags OCaml.DoClause
instance ToTags OCaml.ElseClause
instance ToTags OCaml.EscapeSequence
instance ToTags OCaml.ExceptionDefinition
instance ToTags OCaml.ExceptionPattern
instance ToTags OCaml.ExpressionItem
instance ToTags OCaml.ExtendedModulePath
instance ToTags OCaml.Extension
instance ToTags OCaml.External
instance ToTags OCaml.FieldDeclaration
instance ToTags OCaml.FieldExpression
instance ToTags OCaml.FieldGetExpression
instance ToTags OCaml.FieldName
instance ToTags OCaml.FieldPath
instance ToTags OCaml.FieldPattern
instance ToTags OCaml.FloatingAttribute
instance ToTags OCaml.ForExpression
instance ToTags OCaml.FunctionExpression
instance ToTags OCaml.FunctionType
instance ToTags OCaml.Functor
instance ToTags OCaml.FunctorType
instance ToTags OCaml.FunExpression
instance ToTags OCaml.Guard
instance ToTags OCaml.HashType
instance ToTags OCaml.IfExpression
instance ToTags OCaml.IncludeModule
instance ToTags OCaml.IncludeModuleType
instance ToTags OCaml.IndexingOperator
instance ToTags OCaml.IndexingOperatorPath
instance ToTags OCaml.InfixExpression
instance ToTags OCaml.InfixOperator
instance ToTags OCaml.InheritanceDefinition
instance ToTags OCaml.InheritanceSpecification
instance ToTags OCaml.InstanceVariableDefinition
instance ToTags OCaml.InstanceVariableExpression
instance ToTags OCaml.InstanceVariableName
instance ToTags OCaml.InstanceVariableSpecification
instance ToTags OCaml.InstantiatedClass
instance ToTags OCaml.InstantiatedClassType
instance ToTags OCaml.ItemAttribute
instance ToTags OCaml.ItemExtension
instance ToTags OCaml.Label
instance ToTags OCaml.LabeledArgument
instance ToTags OCaml.LabelName
instance ToTags OCaml.LazyExpression
instance ToTags OCaml.LazyPattern
instance ToTags OCaml.LetBinding
instance ToTags OCaml.LetClassExpression
instance ToTags OCaml.LetExceptionExpression
instance ToTags OCaml.LetExpression
instance ToTags OCaml.LetModuleExpression
instance ToTags OCaml.LetOpenClassExpression
instance ToTags OCaml.LetOpenClassType
instance ToTags OCaml.LetOpenExpression
instance ToTags OCaml.LetOperator
instance ToTags OCaml.ListExpression
instance ToTags OCaml.ListPattern
instance ToTags OCaml.LocalOpenExpression
instance ToTags OCaml.LocalOpenPattern
instance ToTags OCaml.MatchCase
instance ToTags OCaml.MatchExpression
instance ToTags OCaml.MatchOperator
instance ToTags OCaml.MethodDefinition
instance ToTags OCaml.MethodInvocation
instance ToTags OCaml.MethodName
instance ToTags OCaml.MethodSpecification
instance ToTags OCaml.MethodType
instance ToTags OCaml.ModuleApplication
instance ToTags OCaml.ModuleBinding
instance ToTags OCaml.ModuleDefinition
instance ToTags OCaml.ModuleName
instance ToTags OCaml.ModuleParameter
instance ToTags OCaml.ModulePath
instance ToTags OCaml.ModuleTypeConstraint
instance ToTags OCaml.ModuleTypeDefinition
instance ToTags OCaml.ModuleTypeName
instance ToTags OCaml.ModuleTypeOf
instance ToTags OCaml.ModuleTypePath
instance ToTags OCaml.NewExpression
instance ToTags OCaml.Number
instance ToTags OCaml.ObjectCopyExpression
instance ToTags OCaml.ObjectExpression
instance ToTags OCaml.ObjectType
instance ToTags OCaml.OcamlyaccValue
instance ToTags OCaml.OpenModule
instance ToTags OCaml.OperatorName
instance ToTags OCaml.OrPattern
instance ToTags OCaml.PackageExpression
instance ToTags OCaml.PackagePattern
instance ToTags OCaml.PackageType
instance ToTags OCaml.PackedModule
instance ToTags OCaml.Parameter
instance ToTags OCaml.ParenthesizedClassExpression
instance ToTags OCaml.ParenthesizedExpression
instance ToTags OCaml.ParenthesizedModuleExpression
instance ToTags OCaml.ParenthesizedModuleType
instance ToTags OCaml.ParenthesizedPattern
instance ToTags OCaml.ParenthesizedType
instance ToTags OCaml.PolymorphicType
instance ToTags OCaml.PolymorphicVariantPattern
instance ToTags OCaml.PolymorphicVariantType
instance ToTags OCaml.PrefixExpression
instance ToTags OCaml.PrefixOperator
instance ToTags OCaml.PrettyPrintingIndication
instance ToTags OCaml.ProductExpression
instance ToTags OCaml.QuotedExtension
instance ToTags OCaml.QuotedItemExtension
instance ToTags OCaml.QuotedString
instance ToTags OCaml.RangePattern
instance ToTags OCaml.RecordDeclaration
instance ToTags OCaml.RecordExpression
instance ToTags OCaml.RecordPattern
instance ToTags OCaml.RefutationCase
instance ToTags OCaml.SequenceExpression
instance ToTags OCaml.SetExpression
instance ToTags OCaml.Shebang
instance ToTags OCaml.Signature
instance ToTags OCaml.SignedNumber
instance ToTags OCaml.String
instance ToTags OCaml.StringGetExpression
instance ToTags OCaml.Structure
instance ToTags OCaml.SuperArgument
instance ToTags OCaml.SuperClassExpression
instance ToTags OCaml.SuperClassField
instance ToTags OCaml.SuperClassFieldSpecification
instance ToTags OCaml.SuperClassType
instance ToTags OCaml.SuperConstant
instance ToTags OCaml.SuperExpression
instance ToTags OCaml.SuperExtension
instance ToTags OCaml.SuperItemExtension
instance ToTags OCaml.SuperModuleExpression
instance ToTags OCaml.SuperModuleType
instance ToTags OCaml.SuperParameter
instance ToTags OCaml.SuperPattern
instance ToTags OCaml.SuperPatternNoExn
instance ToTags OCaml.SuperPolymorphicType
instance ToTags OCaml.SuperSequenceExpression
instance ToTags OCaml.SuperSignedConstant
instance ToTags OCaml.SuperSimpleClassExpression
instance ToTags OCaml.SuperSimpleClassType
instance ToTags OCaml.SuperSimpleExpression
instance ToTags OCaml.SuperSimpleModuleExpression
instance ToTags OCaml.SuperSimplePattern
instance ToTags OCaml.SuperSimpleType
instance ToTags OCaml.SuperStructureItem
instance ToTags OCaml.SuperTagSpec
instance ToTags OCaml.SuperTupleType
instance ToTags OCaml.SuperType
instance ToTags OCaml.SuperValueName
instance ToTags OCaml.Tag
instance ToTags OCaml.TagPattern
instance ToTags OCaml.TagSpecification
instance ToTags OCaml.ThenClause
instance ToTags OCaml.ToplevelDirective
instance ToTags OCaml.TryExpression
instance ToTags OCaml.TuplePattern
instance ToTags OCaml.TupleType
instance ToTags OCaml.TypeBinding
instance ToTags OCaml.TypeConstraint
instance ToTags OCaml.TypeConstructor
instance ToTags OCaml.TypeConstructorPath
instance ToTags OCaml.TypedClassExpression
instance ToTags OCaml.TypeDefinition
instance ToTags OCaml.TypedExpression
instance ToTags OCaml.TypedLabel
instance ToTags OCaml.TypedModuleExpression
instance ToTags OCaml.TypedPattern
instance ToTags OCaml.TypeParameterConstraint
instance ToTags OCaml.TypeVariable
instance ToTags OCaml.Unit
instance ToTags OCaml.ValueDefinition
instance ToTags OCaml.ValueName
instance ToTags OCaml.ValuePath
instance ToTags OCaml.ValueSpecification
instance ToTags OCaml.VariantDeclaration
instance ToTags OCaml.WhileExpression

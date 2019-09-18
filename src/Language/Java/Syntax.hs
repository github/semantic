{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Java.Syntax where

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Prologue
import Tags.Taggable (Taggable)

newtype Import a = Import { imports :: [a]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Import

-- TODO: Implement Eval instance for ArrayType
instance Evaluatable Import

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Module

instance Evaluatable Module

newtype Package a = Package { packages :: [a]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Package

-- TODO: Implement Eval instance for ArrayType
instance Evaluatable Package

data EnumDeclaration a = EnumDeclaration { enumDeclarationModifier :: ![a], enumDeclarationIdentifier :: !a, enumDeclarationSuperInterfaces :: ![a], enumDeclarationConstant :: ![a], enumDeclarationBody :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically EnumDeclaration

instance Evaluatable EnumDeclaration

data Variable a = Variable { variableModifiers :: ![a], variableType :: !a, variableName :: !a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Variable

-- TODO: Implement Eval instance for Variable
instance Evaluatable Variable

data Synchronized a = Synchronized { synchronizedSubject :: !a, synchronizedBody :: !a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Synchronized

-- TODO: Implement Eval instance for Synchronized
instance Evaluatable Synchronized

data New a = New { newType :: !a, newArgs :: ![a], newClassBody :: Maybe a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically New

-- TODO: Implement Eval instance for New
instance Evaluatable New

data Asterisk a = Asterisk
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Asterisk

-- TODO: Implement Eval instance for New
instance Evaluatable Asterisk

data Constructor a = Constructor { constructorModifiers :: ![a], constructorTypeParams :: ![a], constructorIdentifier :: !a, constructorParams :: ![a], constructorThrows :: ![a], constructorBody :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Constructor

-- TODO: Implement Eval instance for Constructor
instance Evaluatable Constructor

data TypeParameter a = TypeParameter { typeParamAnnotation :: ![a], typeParamIdentifier :: !a, typeParamTypeBound :: ![a]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeParameter

-- TODO: Implement Eval instance for TypeParameter
instance Evaluatable TypeParameter

data Annotation a = Annotation { annotationName :: !a, annotationField :: [a]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Annotation

-- TODO: Implement Eval instance for Annotation
instance Evaluatable Annotation

data AnnotationField a = AnnotationField { annotationFieldName :: a, annotationFieldValue :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically AnnotationField

-- TODO: Implement Eval instance for AnnotationField
instance Evaluatable AnnotationField

data GenericType a = GenericType { genericTypeIdentifier :: a, genericTypeArguments :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically GenericType

-- TODO: Implement Eval instance for GenericType
instance Evaluatable GenericType

data AnnotatedType a = AnnotatedType { annotationes :: [a], annotatedType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically AnnotatedType

-- TODO: Implement Eval instance for AnnotatedType
instance Evaluatable AnnotatedType

newtype CatchType a = CatchType { types :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically CatchType

-- TODO: Implement Eval instance for CatchType
instance Evaluatable CatchType

data TypeWithModifiers a = TypeWithModifiers { types :: [a], modifier :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeWithModifiers

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable TypeWithModifiers

data Wildcard a = Wildcard { wildcardAnnotation :: [a], wildcardBounds :: Maybe a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Wildcard

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable Wildcard

data WildcardBounds a = WildcardBoundExtends { wildcardBoundExtendsType :: a} | WildcardBoundSuper { wildcardBoundSuperType :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically WildcardBounds

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable WildcardBounds

newtype SpreadParameter a = SpreadParameter { spreadParameterVariableDeclarator :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically SpreadParameter

-- TODO: Implement Eval instance for SpreadParameter
instance Evaluatable SpreadParameter

newtype StaticInitializer a = StaticInitializer { staticInitializerBlock :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically StaticInitializer

instance Evaluatable StaticInitializer

data MethodReference a = MethodReference { methodReferenceType :: !a, methodReferenceTypeArgs :: ![a], methodReferenceIdentifier :: !a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically MethodReference

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable MethodReference

data NewKeyword a = NewKeyword
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically NewKeyword

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable NewKeyword

data Lambda a = Lambda { lambdaParams :: ![a], lambdaBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Lambda

instance Evaluatable Lambda

newtype LambdaBody a = LambdaBody { lambdaBodyExpression :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically LambdaBody

instance Evaluatable LambdaBody

data ArrayCreationExpression a = ArrayCreationExpression { arrayCreationExpressionType :: !a, arrayCreationExpressionDims :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ArrayCreationExpression

instance Evaluatable ArrayCreationExpression

data DimsExpr a = DimsExpr { dimsExprAnnotation :: ![a], dimsExprExpression :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically DimsExpr

instance Evaluatable DimsExpr

newtype ClassBody a = ClassBody { classBodyExpression :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ClassBody

instance Evaluatable ClassBody

newtype ClassLiteral a = ClassLiteral { classLiteralType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ClassLiteral

instance Evaluatable ClassLiteral

data TryWithResources a = TryWithResources { tryResources :: ![a], tryBody :: !a, tryCatch :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TryWithResources

-- TODO: Implement Eval instance for TryWithResources
instance Evaluatable TryWithResources

data AssertStatement a = AssertStatement { assertLHS :: !a, assertRHS :: !(Maybe a) }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically AssertStatement

-- TODO: Implement Eval instance for AssertStatement
instance Evaluatable AssertStatement

newtype DefaultValue a = DefaultValue { defaultValueElement :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically DefaultValue

instance Evaluatable DefaultValue

data AnnotationTypeElement a = AnnotationTypeElement { modifiers :: ![a], annotationType :: a, identifier :: !a, dims :: ![a], defaultValue :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically AnnotationTypeElement

-- TODO: Implement Eval instance for AnnotationTypeElement
instance Evaluatable AnnotationTypeElement

{-# LANGUAGE DeriveAnyClass #-}
module Language.Java.Syntax where

import Data.Abstract.Evaluatable
import Diffing.Algorithm
import Prologue hiding (Constructor)
import Data.JSON.Fields

newtype Import a = Import [a]
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)


instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ArrayType
instance Evaluatable Import

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)


instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module

newtype Package a = Package [a]
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)


instance Eq1 Package where liftEq = genericLiftEq
instance Ord1 Package where liftCompare = genericLiftCompare
instance Show1 Package where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for ArrayType
instance Evaluatable Package

data EnumDeclaration a = EnumDeclaration { enumDeclarationModifier :: ![a], enumDeclarationIdentifier :: !a, enumDeclarationSuperInterfaces :: ![a], enumDeclarationConstant :: ![a], enumDeclarationBody :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 EnumDeclaration where liftEq = genericLiftEq
instance Ord1 EnumDeclaration where liftCompare = genericLiftCompare
instance Show1 EnumDeclaration where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable EnumDeclaration


data Variable a = Variable { variableModifiers :: ![a], variableType :: !a, variableName :: !a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Variable where liftEq = genericLiftEq
instance Ord1 Variable where liftCompare = genericLiftCompare
instance Show1 Variable where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Variable
instance Evaluatable Variable

data Synchronized a = Synchronized { synchronizedSubject :: !a, synchronizedBody :: !a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Synchronized where liftEq = genericLiftEq
instance Ord1 Synchronized where liftCompare = genericLiftCompare
instance Show1 Synchronized where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Synchronized
instance Evaluatable Synchronized

data New a = New { newType :: !a, newArgs :: ![a], newClassBody :: Maybe a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 New where liftEq = genericLiftEq
instance Ord1 New where liftCompare = genericLiftCompare
instance Show1 New where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for New
instance Evaluatable New

data Asterisk a = Asterisk
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Asterisk where liftEq = genericLiftEq
instance Ord1 Asterisk where liftCompare = genericLiftCompare
instance Show1 Asterisk where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for New
instance Evaluatable Asterisk


data Constructor a = Constructor { constructorModifiers :: ![a], constructorTypeParams :: ![a], constructorIdentifier :: !a, constructorParams :: ![a], constructorThrows :: ![a], constructorBody :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Constructor where liftEq = genericLiftEq
instance Ord1 Constructor where liftCompare = genericLiftCompare
instance Show1 Constructor where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Constructor
instance Evaluatable Constructor

data TypeParameter a = TypeParameter { typeParamAnnotation :: ![a], typeParamIdentifier :: !a, typeParamTypeBound :: ![a]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeParameter where liftEq = genericLiftEq
instance Ord1 TypeParameter where liftCompare = genericLiftCompare
instance Show1 TypeParameter where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeParameter
instance Evaluatable TypeParameter

data Annotation a = Annotation { annotationName :: !a, annotationField :: [a]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Annotation
instance Evaluatable Annotation

data AnnotationField a = AnnotationField { annotationFieldName :: a, annotationFieldValue :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AnnotationField where liftEq = genericLiftEq
instance Ord1 AnnotationField where liftCompare = genericLiftCompare
instance Show1 AnnotationField where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for AnnotationField
instance Evaluatable AnnotationField

data GenericType a = GenericType { genericTypeIdentifier :: a, genericTypeArguments :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 GenericType where liftEq = genericLiftEq
instance Ord1 GenericType where liftCompare = genericLiftCompare
instance Show1 GenericType where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for GenericType
instance Evaluatable GenericType

data TypeWithModifiers a = TypeWithModifiers [a] a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeWithModifiers where liftEq = genericLiftEq
instance Ord1 TypeWithModifiers where liftCompare = genericLiftCompare
instance Show1 TypeWithModifiers where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable TypeWithModifiers

data Wildcard a = Wildcard { wildcardAnnotation :: [a], wildcardBounds :: Maybe a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Wildcard where liftEq = genericLiftEq
instance Ord1 Wildcard where liftCompare = genericLiftCompare
instance Show1 Wildcard where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable Wildcard

data WildcardBounds a = WildcardBoundExtends { wildcardBoundType :: a} | WildcardBoundSuper { wildcardBoundType :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 WildcardBounds where liftEq = genericLiftEq
instance Ord1 WildcardBounds where liftCompare = genericLiftCompare
instance Show1 WildcardBounds where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable WildcardBounds

newtype SpreadParameter a = SpreadParameter { spreadParameterVariableDeclarator :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 SpreadParameter where liftEq = genericLiftEq
instance Ord1 SpreadParameter where liftCompare = genericLiftCompare
instance Show1 SpreadParameter where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for SpreadParameter
instance Evaluatable SpreadParameter

data StaticInitializer a = StaticInitializer { staticInitializerBlock :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)


instance Eq1 StaticInitializer where liftEq = genericLiftEq
instance Ord1 StaticInitializer where liftCompare = genericLiftCompare
instance Show1 StaticInitializer where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StaticInitializer

-- I think this is wrong because it doesn't acknowledge double colon.
data MethodReference a = MethodReference { methodReferenceType :: !a, methodReferenceTypeArgs :: ![a], methodReferenceIdentifier :: !a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 MethodReference where liftEq = genericLiftEq
instance Ord1 MethodReference where liftCompare = genericLiftCompare
instance Show1 MethodReference where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable MethodReference

data NewKeyword a = NewKeyword
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 NewKeyword where liftEq = genericLiftEq
instance Ord1 NewKeyword where liftCompare = genericLiftCompare
instance Show1 NewKeyword where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeWithModifiers
instance Evaluatable NewKeyword

data Lambda a = Lambda { lambdaParams :: ![a], lambdaBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Lambda where liftEq = genericLiftEq
instance Ord1 Lambda where liftCompare = genericLiftCompare
instance Show1 Lambda where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Lambda

data LambdaBody a = LambdaBody { lambdaBodyExpression :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 LambdaBody where liftEq = genericLiftEq
instance Ord1 LambdaBody where liftCompare = genericLiftCompare
instance Show1 LambdaBody where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LambdaBody

data ArrayCreationExpression a = ArrayCreationExpression { arrayCreationExpressionType :: !a, arrayCreationExpressionDims :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ArrayCreationExpression where liftEq = genericLiftEq
instance Ord1 ArrayCreationExpression where liftCompare = genericLiftCompare
instance Show1 ArrayCreationExpression where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ArrayCreationExpression

data DimsExpr a = DimsExpr { dimsExprAnnotation :: ![a], dimsExprExpression :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 DimsExpr where liftEq = genericLiftEq
instance Ord1 DimsExpr where liftCompare = genericLiftCompare
instance Show1 DimsExpr where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DimsExpr

newtype ClassBody a = ClassBody { classBodyExpression :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ClassBody where liftEq = genericLiftEq
instance Ord1 ClassBody where liftCompare = genericLiftCompare
instance Show1 ClassBody where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ClassBody

newtype ClassLiteral a = ClassLiteral { classLiteralType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ClassLiteral where liftEq = genericLiftEq
instance Ord1 ClassLiteral where liftCompare = genericLiftCompare
instance Show1 ClassLiteral where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ClassLiteral

data TryWithResources a = TryWithResources { tryResources :: ![a], tryBody :: !a, tryCatch :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TryWithResources where liftEq = genericLiftEq
instance Ord1 TryWithResources where liftCompare = genericLiftCompare
instance Show1 TryWithResources where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TryWithResources
instance Evaluatable TryWithResources

data AssertStatement a = AssertStatement { assertLHS :: !a, assertRHS :: !(Maybe a) }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AssertStatement where liftEq = genericLiftEq
instance Ord1 AssertStatement where liftCompare = genericLiftCompare
instance Show1 AssertStatement where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for AssertStatement
instance Evaluatable AssertStatement

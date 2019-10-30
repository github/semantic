{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeApplications, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Abstract.AccessControls.Instances () where

import Data.Sum
import Data.Term
import Data.Abstract.AccessControls.Class

import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Directive as Directive
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Type as Type
import qualified Language.Go.Syntax as Go
import qualified Language.Go.Term as Go
import qualified Language.Go.Type as Go
import qualified Language.PHP.Syntax as PHP
import qualified Language.PHP.Term as PHP
import qualified Language.Python.Syntax as Python
import qualified Language.Python.Term as Python
import qualified Language.Ruby.Syntax as Ruby
import qualified Language.Ruby.Term as Ruby
import qualified Language.TSX.Syntax as TSX
import qualified Language.TSX.Term as TSX
import qualified Language.TypeScript.Syntax as TypeScript
import qualified Language.TypeScript.Term as TypeScript
import Data.Quieterm

deriving instance AccessControls1 syntax => AccessControls (Term syntax ann)
deriving instance AccessControls (Go.Term ann)
deriving instance AccessControls (PHP.Term ann)
deriving instance AccessControls (Python.Term ann)
deriving instance AccessControls (Ruby.Term ann)
deriving instance AccessControls (TSX.Term ann)
deriving instance AccessControls (TypeScript.Term ann)

instance (AccessControls recur, AccessControls1 syntax) => AccessControls (TermF syntax ann recur) where
  termToAccessControl = liftTermToAccessControl termToAccessControl . termFOut

instance Apply AccessControls1 fs => AccessControls1 (Sum fs) where
  liftTermToAccessControl f = apply @AccessControls1 (liftTermToAccessControl f)

deriving instance AccessControls1 syntax => AccessControls (Quieterm syntax ann)

instance AccessControls1 []
instance AccessControls1 Comment.Comment
instance AccessControls1 Comment.HashBang

instance AccessControls1 Expression.And
instance AccessControls1 Expression.Await
instance AccessControls1 Expression.BAnd
instance AccessControls1 Expression.BOr
instance AccessControls1 Expression.BXOr
instance AccessControls1 Expression.Call
instance AccessControls1 Expression.Cast
instance AccessControls1 Expression.Comparison
instance AccessControls1 Expression.Complement
instance AccessControls1 Expression.Delete
instance AccessControls1 Expression.DividedBy
instance AccessControls1 Expression.Enumeration
instance AccessControls1 Expression.Equal
instance AccessControls1 Expression.FloorDivision
instance AccessControls1 Expression.GreaterThan
instance AccessControls1 Expression.GreaterThanEqual
instance AccessControls1 Expression.InstanceOf
instance AccessControls1 Expression.LessThan
instance AccessControls1 Expression.LessThanEqual
instance AccessControls1 Expression.LShift
instance AccessControls1 Expression.Matches
instance AccessControls1 Expression.Member
instance AccessControls1 Expression.MemberAccess
instance AccessControls1 Expression.Minus
instance AccessControls1 Expression.Modulo
instance AccessControls1 Expression.Negate
instance AccessControls1 Expression.New
instance AccessControls1 Expression.NonNullExpression
instance AccessControls1 Expression.Not
instance AccessControls1 Expression.NotMatches
instance AccessControls1 Expression.Or
instance AccessControls1 Expression.Plus
instance AccessControls1 Expression.Power
instance AccessControls1 Expression.RShift
instance AccessControls1 Expression.ScopeResolution
instance AccessControls1 Expression.SequenceExpression
instance AccessControls1 Expression.StrictEqual
instance AccessControls1 Expression.Subscript
instance AccessControls1 Expression.Super
instance AccessControls1 Expression.Times
instance AccessControls1 Expression.Typeof
instance AccessControls1 Expression.UnsignedRShift
instance AccessControls1 Expression.Void
instance AccessControls1 Expression.XOr

instance AccessControls1 Literal.Boolean
instance AccessControls1 Literal.Integer
instance AccessControls1 Literal.Float
instance AccessControls1 Literal.Rational
instance AccessControls1 Literal.Complex
instance AccessControls1 Literal.String
instance AccessControls1 Literal.Character
instance AccessControls1 Literal.InterpolationElement
instance AccessControls1 Literal.TextElement
instance AccessControls1 Literal.EscapeSequence
instance AccessControls1 Literal.Symbol
instance AccessControls1 Literal.SymbolElement
instance AccessControls1 Literal.Regex
instance AccessControls1 Literal.Array
instance AccessControls1 Literal.Hash
instance AccessControls1 Literal.Tuple
instance AccessControls1 Literal.Set
instance AccessControls1 Literal.Pointer
instance AccessControls1 Literal.Reference
instance AccessControls1 Literal.Null
instance AccessControls1 Literal.KeyValue

instance AccessControls1 Statement.Assignment
instance AccessControls1 Statement.Break
instance AccessControls1 Statement.Catch
instance AccessControls1 Statement.Continue
instance AccessControls1 Statement.DoWhile
instance AccessControls1 Statement.Else
instance AccessControls1 Statement.Finally
instance AccessControls1 Statement.For
instance AccessControls1 Statement.ForEach
instance AccessControls1 Statement.Goto
instance AccessControls1 Statement.If
instance AccessControls1 Statement.Let
instance AccessControls1 Statement.Match
instance AccessControls1 Statement.NoOp
instance AccessControls1 Statement.Pattern
instance AccessControls1 Statement.PostDecrement
instance AccessControls1 Statement.PostIncrement
instance AccessControls1 Statement.PreDecrement
instance AccessControls1 Statement.PreIncrement
instance AccessControls1 Statement.Retry
instance AccessControls1 Statement.Return
instance AccessControls1 Statement.ScopeEntry
instance AccessControls1 Statement.ScopeExit
instance AccessControls1 Statement.StatementBlock
instance AccessControls1 Statement.Statements
instance AccessControls1 Statement.Throw
instance AccessControls1 Statement.Try
instance AccessControls1 Statement.While
instance AccessControls1 Statement.Yield

instance AccessControls1 Syntax.Context
instance AccessControls1 Syntax.Empty
instance AccessControls1 Syntax.Error
instance AccessControls1 Syntax.Identifier
instance AccessControls1 Syntax.AccessibilityModifier

instance AccessControls1 Type.Annotation
instance AccessControls1 Type.Array
instance AccessControls1 Type.Bool
instance AccessControls1 Type.Double
instance AccessControls1 Type.Float
instance AccessControls1 Type.Function
instance AccessControls1 Type.Int
instance AccessControls1 Type.Interface
instance AccessControls1 Type.Map
instance AccessControls1 Type.Parenthesized
instance AccessControls1 Type.Pointer
instance AccessControls1 Type.Product
instance AccessControls1 Type.Readonly
instance AccessControls1 Type.Slice
instance AccessControls1 Type.TypeParameters
instance AccessControls1 Type.Void

instance AccessControls1 Declaration.Class
instance AccessControls1 Declaration.Comprehension
instance AccessControls1 Declaration.Constructor
instance AccessControls1 Declaration.Datatype
instance AccessControls1 Declaration.Decorator
instance AccessControls1 Declaration.Function
instance AccessControls1 Declaration.InterfaceDeclaration
instance AccessControls1 Declaration.Method
instance AccessControls1 Declaration.MethodSignature
instance AccessControls1 Declaration.OptionalParameter
instance AccessControls1 Declaration.PublicFieldDefinition
instance AccessControls1 Declaration.RequiredParameter
instance AccessControls1 Declaration.Type
instance AccessControls1 Declaration.TypeAlias
instance AccessControls1 Declaration.Variable
instance AccessControls1 Declaration.VariableDeclaration

instance AccessControls1 Directive.File
instance AccessControls1 Directive.Line

instance AccessControls1 Python.Alias
instance AccessControls1 Python.Ellipsis
instance AccessControls1 Python.FutureImport
instance AccessControls1 Python.Import
instance AccessControls1 Python.QualifiedAliasedImport
instance AccessControls1 Python.QualifiedImport
instance AccessControls1 Python.Redirect

instance AccessControls1 Go.BidirectionalChannel
instance AccessControls1 Go.ReceiveChannel
instance AccessControls1 Go.SendChannel
instance AccessControls1 Go.Import
instance AccessControls1 Go.QualifiedImport
instance AccessControls1 Go.SideEffectImport
instance AccessControls1 Go.Composite
instance AccessControls1 Go.Label
instance AccessControls1 Go.Send
instance AccessControls1 Go.Slice
instance AccessControls1 Go.TypeSwitch
instance AccessControls1 Go.Receive
instance AccessControls1 Go.Field
instance AccessControls1 Go.Package
instance AccessControls1 Go.TypeAssertion
instance AccessControls1 Go.TypeConversion
instance AccessControls1 Go.Variadic
instance AccessControls1 Go.DefaultPattern
instance AccessControls1 Go.Defer
instance AccessControls1 Go.Go
instance AccessControls1 Go.Rune
instance AccessControls1 Go.Select
instance AccessControls1 Go.TypeSwitchGuard
instance AccessControls1 Go.ReceiveOperator

instance AccessControls1 PHP.Text
instance AccessControls1 PHP.VariableName
instance AccessControls1 PHP.Require
instance AccessControls1 PHP.RequireOnce
instance AccessControls1 PHP.Include
instance AccessControls1 PHP.IncludeOnce
instance AccessControls1 PHP.ArrayElement
instance AccessControls1 PHP.GlobalDeclaration
instance AccessControls1 PHP.SimpleVariable
instance AccessControls1 PHP.Concat
instance AccessControls1 PHP.CastType
instance AccessControls1 PHP.ErrorControl
instance AccessControls1 PHP.Clone
instance AccessControls1 PHP.ShellCommand
instance AccessControls1 PHP.Update
instance AccessControls1 PHP.NewVariable
instance AccessControls1 PHP.RelativeScope
instance AccessControls1 PHP.NamespaceName
instance AccessControls1 PHP.ConstDeclaration
instance AccessControls1 PHP.ClassInterfaceClause
instance AccessControls1 PHP.ClassBaseClause
instance AccessControls1 PHP.UseClause
instance AccessControls1 PHP.ReturnType
instance AccessControls1 PHP.TypeDeclaration
instance AccessControls1 PHP.BaseTypeDeclaration
instance AccessControls1 PHP.ScalarType
instance AccessControls1 PHP.EmptyIntrinsic
instance AccessControls1 PHP.ExitIntrinsic
instance AccessControls1 PHP.IssetIntrinsic
instance AccessControls1 PHP.EvalIntrinsic
instance AccessControls1 PHP.PrintIntrinsic
instance AccessControls1 PHP.NamespaceAliasingClause
instance AccessControls1 PHP.NamespaceUseDeclaration
instance AccessControls1 PHP.NamespaceUseClause
instance AccessControls1 PHP.NamespaceUseGroupClause
instance AccessControls1 PHP.TraitUseSpecification
instance AccessControls1 PHP.Static
instance AccessControls1 PHP.ClassModifier
instance AccessControls1 PHP.InterfaceBaseClause
instance AccessControls1 PHP.Echo
instance AccessControls1 PHP.Unset
instance AccessControls1 PHP.DeclareDirective
instance AccessControls1 PHP.LabeledStatement
instance AccessControls1 PHP.QualifiedName
instance AccessControls1 PHP.ClassConstDeclaration
instance AccessControls1 PHP.Namespace
instance AccessControls1 PHP.TraitDeclaration
instance AccessControls1 PHP.AliasAs
instance AccessControls1 PHP.InsteadOf
instance AccessControls1 PHP.TraitUseClause
instance AccessControls1 PHP.DestructorDeclaration
instance AccessControls1 PHP.ConstructorDeclaration
instance AccessControls1 PHP.PropertyDeclaration
instance AccessControls1 PHP.PropertyModifier
instance AccessControls1 PHP.InterfaceDeclaration
instance AccessControls1 PHP.Declare

instance AccessControls1 Ruby.Assignment
instance AccessControls1 Ruby.Class
instance AccessControls1 Ruby.Send
instance AccessControls1 Ruby.Require
instance AccessControls1 Ruby.Load
instance AccessControls1 Ruby.LowPrecedenceAnd
instance AccessControls1 Ruby.LowPrecedenceOr
instance AccessControls1 Ruby.Module
instance AccessControls1 Ruby.ZSuper

instance AccessControls1 TSX.JsxElement
instance AccessControls1 TSX.JsxOpeningElement
instance AccessControls1 TSX.JsxSelfClosingElement
instance AccessControls1 TSX.JsxAttribute
instance AccessControls1 TSX.JsxNamespaceName
instance AccessControls1 TSX.JsxText
instance AccessControls1 TSX.JsxExpression
instance AccessControls1 TSX.JsxClosingElement
instance AccessControls1 TSX.JsxFragment

instance AccessControls1 TypeScript.AnnotatedExpression
instance AccessControls1 TypeScript.JavaScriptRequire
instance AccessControls1 TypeScript.Debugger
instance AccessControls1 TypeScript.Super
instance AccessControls1 TypeScript.Undefined
instance AccessControls1 TypeScript.With
instance AccessControls1 TypeScript.OptionalParameter
instance AccessControls1 TypeScript.RequiredParameter
instance AccessControls1 TypeScript.RestParameter
instance AccessControls1 TypeScript.ImplementsClause
instance AccessControls1 TypeScript.Import
instance AccessControls1 TypeScript.QualifiedAliasedImport
instance AccessControls1 TypeScript.QualifiedExportFrom
instance AccessControls1 TypeScript.LookupType
instance AccessControls1 TypeScript.Union
instance AccessControls1 TypeScript.Intersection
instance AccessControls1 TypeScript.FunctionType
instance AccessControls1 TypeScript.AmbientFunction
instance AccessControls1 TypeScript.ImportRequireClause
instance AccessControls1 TypeScript.Constructor
instance AccessControls1 TypeScript.TypeParameter
instance AccessControls1 TypeScript.TypeAssertion
instance AccessControls1 TypeScript.NestedIdentifier
instance AccessControls1 TypeScript.NestedTypeIdentifier
instance AccessControls1 TypeScript.GenericType
instance AccessControls1 TypeScript.TypePredicate
instance AccessControls1 TypeScript.EnumDeclaration
instance AccessControls1 TypeScript.PropertySignature
instance AccessControls1 TypeScript.CallSignature
instance AccessControls1 TypeScript.ConstructSignature
instance AccessControls1 TypeScript.IndexSignature
instance AccessControls1 TypeScript.AbstractMethodSignature
instance AccessControls1 TypeScript.ForOf
instance AccessControls1 TypeScript.LabeledStatement
instance AccessControls1 TypeScript.InternalModule
instance AccessControls1 TypeScript.ImportAlias
instance AccessControls1 TypeScript.ClassHeritage
instance AccessControls1 TypeScript.AbstractClass
instance AccessControls1 TypeScript.SideEffectImport
instance AccessControls1 TypeScript.QualifiedExport
instance AccessControls1 TypeScript.DefaultExport
instance AccessControls1 TypeScript.ShorthandPropertyIdentifier
instance AccessControls1 TypeScript.ImportClause
instance AccessControls1 TypeScript.Tuple
instance AccessControls1 TypeScript.Annotation
instance AccessControls1 TypeScript.Decorator
instance AccessControls1 TypeScript.ComputedPropertyName
instance AccessControls1 TypeScript.Constraint
instance AccessControls1 TypeScript.DefaultType
instance AccessControls1 TypeScript.ParenthesizedType
instance AccessControls1 TypeScript.PredefinedType
instance AccessControls1 TypeScript.TypeIdentifier
instance AccessControls1 TypeScript.ObjectType
instance AccessControls1 TypeScript.AmbientDeclaration
instance AccessControls1 TypeScript.ExtendsClause
instance AccessControls1 TypeScript.ArrayType
instance AccessControls1 TypeScript.FlowMaybeType
instance AccessControls1 TypeScript.TypeQuery
instance AccessControls1 TypeScript.IndexTypeQuery
instance AccessControls1 TypeScript.TypeArguments
instance AccessControls1 TypeScript.ThisType
instance AccessControls1 TypeScript.ExistentialType
instance AccessControls1 TypeScript.LiteralType
instance AccessControls1 TypeScript.Update
instance AccessControls1 TypeScript.MetaProperty
instance AccessControls1 TypeScript.Module

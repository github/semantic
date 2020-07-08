{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.PHP.Tags
  ( tags,
  )
where

import AST.Element
import qualified AST.Parse as Parse
import AST.Token
import AST.Traversable1
import Control.Effect.Reader
import Control.Effect.Writer
import Control.Effect.State
import qualified Language.PHP.AST as PHP
import Proto.Semantic as P
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

instance ToTags PHP.FunctionDefinition where
  tags
    t@PHP.FunctionDefinition
      { PHP.ann = Loc {byteRange},
        PHP.name = Parse.Success (PHP.Name {text, ann})
      } = Tags.yield text P.METHOD P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags PHP.MethodDeclaration where
  tags
    t@PHP.MethodDeclaration
      { PHP.ann = Loc {byteRange},
        PHP.name = Parse.Success (PHP.Name {text, ann})
      } = Tags.yield text P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags PHP.FunctionCallExpression where
  tags
    t@PHP.FunctionCallExpression
      { PHP.ann = Loc {byteRange},
        PHP.function = func
      } = match func
      where
        yield name loc = Tags.yield name P.CALL P.REFERENCE loc byteRange >> gtags t
        match expr = case expr of
          EPrj PHP.VariableName {extraChildren = Parse.Success (PHP.Name {text, ann})} -> yield text ann *> gtags t
          EPrj PHP.QualifiedName {extraChildren = [EPrj PHP.Name {text, ann}]} -> yield text ann *> gtags t
          _ -> gtags t


instance ToTags PHP.MemberCallExpression where
  tags
    t@PHP.MemberCallExpression
      { PHP.ann = Loc {byteRange},
        PHP.name = Parse.Success (Prj PHP.Name {text, ann})
      } = Tags.yield text P.CALL P.REFERENCE ann byteRange >> gtags t
  tags t = gtags t



instance ToTags PHP.AnonymousFunctionCreationExpression
instance ToTags PHP.AnonymousFunctionUseClause
instance ToTags PHP.Arguments
instance ToTags PHP.ArrayCreationExpression
instance ToTags PHP.ArrayElementInitializer
instance ToTags PHP.AssignmentExpression
instance ToTags PHP.AugmentedAssignmentExpression
instance ToTags PHP.BinaryExpression
instance ToTags PHP.Boolean
instance ToTags PHP.BreakStatement
instance ToTags PHP.CaseStatement
instance ToTags PHP.CastExpression
instance ToTags PHP.CastType
instance ToTags PHP.CatchClause
instance ToTags PHP.ClassBaseClause
instance ToTags PHP.ClassConstantAccessExpression
instance ToTags PHP.ClassDeclaration
instance ToTags PHP.ClassInterfaceClause
instance ToTags PHP.ClassModifier
instance ToTags PHP.CloneExpression
instance ToTags PHP.ColonBlock
instance ToTags PHP.CompoundStatement
instance ToTags PHP.ConditionalExpression
instance ToTags PHP.ConstDeclaration
instance ToTags PHP.ConstElement
instance ToTags PHP.ContinueStatement
instance ToTags PHP.DeclarationList
instance ToTags PHP.DeclareDirective
instance ToTags PHP.DeclareStatement
instance ToTags PHP.DefaultStatement
instance ToTags PHP.DoStatement
instance ToTags PHP.DynamicVariableName
instance ToTags PHP.EchoStatement
instance ToTags PHP.ElseClause
instance ToTags PHP.ElseIfClause
instance ToTags PHP.EmptyStatement
instance ToTags PHP.ExponentiationExpression
instance ToTags PHP.Expression
instance ToTags PHP.ExpressionStatement
instance ToTags PHP.FinallyClause
instance ToTags PHP.Float
instance ToTags PHP.ForStatement
instance ToTags PHP.ForeachStatement
instance ToTags PHP.FormalParameters
instance ToTags PHP.FunctionStaticDeclaration
instance ToTags PHP.GlobalDeclaration
instance ToTags PHP.GotoStatement
instance ToTags PHP.Heredoc
instance ToTags PHP.IfStatement
instance ToTags PHP.IncludeExpression
instance ToTags PHP.IncludeOnceExpression
instance ToTags PHP.InterfaceBaseClause
instance ToTags PHP.InterfaceDeclaration
instance ToTags PHP.ListLiteral
instance ToTags PHP.Literal
instance ToTags PHP.MemberAccessExpression
instance ToTags PHP.Name
instance ToTags PHP.NamedLabelStatement
instance ToTags PHP.NamespaceAliasingClause
instance ToTags PHP.NamespaceDefinition
instance ToTags PHP.NamespaceFunctionOrConst
instance ToTags PHP.NamespaceName
instance ToTags PHP.NamespaceNameAsPrefix
instance ToTags PHP.NamespaceUseClause
instance ToTags PHP.NamespaceUseDeclaration
instance ToTags PHP.NamespaceUseGroup
instance ToTags PHP.NamespaceUseGroupClause
instance ToTags PHP.Null
instance ToTags PHP.ObjectCreationExpression
instance ToTags PHP.OptionalType
instance ToTags PHP.Pair
instance ToTags PHP.ParenthesizedExpression
instance ToTags PHP.PhpTag
instance ToTags PHP.PrimaryExpression
instance ToTags PHP.PrimitiveType
instance ToTags PHP.PrintIntrinsic
instance ToTags PHP.Program
instance ToTags PHP.PropertyDeclaration
instance ToTags PHP.PropertyElement
instance ToTags PHP.PropertyInitializer
instance ToTags PHP.QualifiedName
instance ToTags PHP.RelativeScope
instance ToTags PHP.RequireExpression
instance ToTags PHP.RequireOnceExpression
instance ToTags PHP.ReturnStatement
instance ToTags PHP.ScopedCallExpression
instance ToTags PHP.ScopedPropertyAccessExpression
instance ToTags PHP.SequenceExpression
instance ToTags PHP.ShellCommandExpression
instance ToTags PHP.SimpleParameter
instance ToTags PHP.Statement
instance ToTags PHP.StaticModifier
instance ToTags PHP.StaticVariableDeclaration
instance ToTags PHP.String
instance ToTags PHP.SubscriptExpression
instance ToTags PHP.SwitchBlock
instance ToTags PHP.SwitchStatement
instance ToTags PHP.Text
instance ToTags PHP.ThrowStatement
instance ToTags PHP.TraitDeclaration
instance ToTags PHP.TryStatement
instance ToTags PHP.Type
instance ToTags PHP.TypeName
instance ToTags PHP.UnaryOpExpression
instance ToTags PHP.UnsetStatement
instance ToTags PHP.UpdateExpression
instance ToTags PHP.UseAsClause
instance ToTags PHP.UseDeclaration
instance ToTags PHP.UseInsteadOfClause
instance ToTags PHP.UseList
instance ToTags PHP.VarModifier
instance ToTags PHP.VariableName
instance ToTags PHP.VariadicParameter
instance ToTags PHP.VariadicUnpacking
instance ToTags PHP.VisibilityModifier
instance ToTags PHP.WhileStatement
instance ToTags PHP.YieldExpression
instance ToTags PHP.Integer

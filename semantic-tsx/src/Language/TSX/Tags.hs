{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Language.TSX.Tags
( ToTags(..)
) where

import           AST.Element
import           AST.Token
import           AST.Traversable1
import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Foldable
import           Data.Text as Text
import qualified Language.TSX.AST as Tsx
import           Source.Loc
import           Source.Source as Source
import           Tags.Tag
import qualified Tags.Tagging.Precise as Tags

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

instance ToTags Tsx.Function where
  tags t@Tsx.Function
    { ann = loc@Loc { byteRange }
    , name = Just Tsx.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t
  tags t = gtags t

instance ToTags Tsx.FunctionSignature where
  tags t@Tsx.FunctionSignature
    { ann = loc@Loc { byteRange }
    , name = Tsx.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTags Tsx.FunctionDeclaration where
  tags t@Tsx.FunctionDeclaration
    { ann = loc@Loc { byteRange }
    , name = Tsx.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTags Tsx.MethodDefinition where
  tags t@Tsx.MethodDefinition
    { ann = loc@Loc { byteRange }
    , name
    } = case name of
      Prj Tsx.PropertyIdentifier { text } -> yield text
      -- TODO: There are more here
      _                                   -> gtags t
      where
        yield name = yieldTag name Method loc byteRange >> gtags t

instance ToTags Tsx.ClassDeclaration where
  tags t@Tsx.ClassDeclaration
    { ann = loc@Loc { byteRange }
    , name = Tsx.TypeIdentifier { text }
    } = yieldTag text Class loc byteRange >> gtags t

instance ToTags Tsx.CallExpression where
  tags t@Tsx.CallExpression
    { ann = loc@Loc { byteRange }
    , function = Tsx.Expression expr
    } = match expr
    where
      match expr = case expr of
        Prj Tsx.Identifier { text } -> yield text
        Prj Tsx.NewExpression { constructor = Prj Tsx.Identifier { text } } -> yield text
        Prj Tsx.CallExpression { function = Tsx.Expression expr } -> match expr
        Prj Tsx.MemberExpression { property = Tsx.PropertyIdentifier { text } } -> yield text
        Prj Tsx.Function { name = Just Tsx.Identifier { text }} -> yield text
        Prj Tsx.ParenthesizedExpression { extraChildren } -> for_ extraChildren $ \ x -> case x of
          Prj (Tsx.Expression expr) -> match expr
          _                         -> tags x
        _ -> gtags t
      yield name = yieldTag name Call loc byteRange >> gtags t

instance ToTags Tsx.Class where
  tags t@Tsx.Class
    { ann = loc@Loc { byteRange }
    , name = Just Tsx.TypeIdentifier { text }
    } = yieldTag text Class loc byteRange >> gtags t
  tags t = gtags t

instance ToTags Tsx.Module where
  tags t@Tsx.Module
    { ann = loc@Loc { byteRange }
    , name
    } = match name
    where
      match expr = case expr of
        Prj Tsx.Identifier { text } -> yield text
        -- TODO: Handle NestedIdentifiers and Strings
        -- Prj Tsx.NestedIdentifier { extraChildren } -> match
        _                           -> gtags t
      yield text = yieldTag text Module loc byteRange >> gtags t

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

-- These are all valid, but point to built-in functions (e.g. require) that a la
-- carte doesn't display and since we have nothing to link to yet (can't
-- jump-to-def), we hide them from the current tags output.
nameBlacklist :: [Text]
nameBlacklist =
  [ "require"
  ]

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> Kind -> Loc -> Range -> m ()
yieldTag name Call _ _ | name `elem` nameBlacklist = pure ()
yieldTag name kind loc range = do
  src <- ask @Source
  Tags.yield (Tag name kind loc (Tags.firstLine src range) Nothing)

instance ToTags Tsx.AbstractClassDeclaration
instance ToTags Tsx.AbstractMethodSignature
instance ToTags Tsx.AccessibilityModifier
instance ToTags Tsx.AmbientDeclaration
instance ToTags Tsx.Arguments
instance ToTags Tsx.Array
instance ToTags Tsx.ArrayPattern
instance ToTags Tsx.ArrayType
instance ToTags Tsx.ArrowFunction
instance ToTags Tsx.AsExpression
instance ToTags Tsx.AssignmentExpression
instance ToTags Tsx.AssignmentPattern
instance ToTags Tsx.AugmentedAssignmentExpression
instance ToTags Tsx.AwaitExpression
instance ToTags Tsx.BinaryExpression
instance ToTags Tsx.BreakStatement
-- instance ToTags Tsx.CallExpression
instance ToTags Tsx.CallSignature
instance ToTags Tsx.CatchClause
-- instance ToTags Tsx.Class
instance ToTags Tsx.ClassBody
-- instance ToTags Tsx.ClassDeclaration
instance ToTags Tsx.ClassHeritage
instance ToTags Tsx.ComputedPropertyName
instance ToTags Tsx.Constraint
instance ToTags Tsx.ConstructSignature
instance ToTags Tsx.ConstructorType
instance ToTags Tsx.ContinueStatement
instance ToTags Tsx.DebuggerStatement
instance ToTags Tsx.Declaration
instance ToTags Tsx.Decorator
instance ToTags Tsx.DefaultType
instance ToTags Tsx.DestructuringPattern
instance ToTags Tsx.DoStatement
instance ToTags Tsx.EmptyStatement
instance ToTags Tsx.EnumAssignment
instance ToTags Tsx.EnumBody
instance ToTags Tsx.EnumDeclaration
instance ToTags Tsx.EscapeSequence
instance ToTags Tsx.ExistentialType
instance ToTags Tsx.ExportClause
instance ToTags Tsx.ExportSpecifier
instance ToTags Tsx.ExportStatement
instance ToTags Tsx.Expression
instance ToTags Tsx.ExpressionStatement
instance ToTags Tsx.ExtendsClause
instance ToTags Tsx.False
instance ToTags Tsx.FinallyClause
instance ToTags Tsx.FlowMaybeType
instance ToTags Tsx.ForInStatement
instance ToTags Tsx.ForStatement
instance ToTags Tsx.FormalParameters
-- instance ToTags Tsx.Function
-- instance ToTags Tsx.FunctionDeclaration
-- instance ToTags Tsx.FunctionSignature
instance ToTags Tsx.FunctionType
instance ToTags Tsx.GeneratorFunction
instance ToTags Tsx.GeneratorFunctionDeclaration
instance ToTags Tsx.GenericType
instance ToTags Tsx.HashBangLine
instance ToTags Tsx.Identifier
instance ToTags Tsx.IfStatement
instance ToTags Tsx.ImplementsClause
instance ToTags Tsx.Import
instance ToTags Tsx.ImportAlias
instance ToTags Tsx.ImportClause
instance ToTags Tsx.ImportRequireClause
instance ToTags Tsx.ImportSpecifier
instance ToTags Tsx.ImportStatement
instance ToTags Tsx.IndexSignature
instance ToTags Tsx.IndexTypeQuery
instance ToTags Tsx.InterfaceDeclaration
instance ToTags Tsx.InternalModule
instance ToTags Tsx.IntersectionType
instance ToTags Tsx.JsxAttribute
instance ToTags Tsx.JsxClosingElement
instance ToTags Tsx.JsxElement
instance ToTags Tsx.JsxExpression
instance ToTags Tsx.JsxFragment
instance ToTags Tsx.JsxNamespaceName
instance ToTags Tsx.JsxOpeningElement
instance ToTags Tsx.JsxSelfClosingElement
instance ToTags Tsx.JsxText
instance ToTags Tsx.LabeledStatement
instance ToTags Tsx.LexicalDeclaration
instance ToTags Tsx.LiteralType
instance ToTags Tsx.LookupType
instance ToTags Tsx.MappedTypeClause
instance ToTags Tsx.MemberExpression
instance ToTags Tsx.MetaProperty
-- instance ToTags Tsx.MethodDefinition
instance ToTags Tsx.MethodSignature
-- instance ToTags Tsx.Module
instance ToTags Tsx.NamedImports
instance ToTags Tsx.NamespaceImport
instance ToTags Tsx.NestedIdentifier
instance ToTags Tsx.NestedTypeIdentifier
instance ToTags Tsx.NewExpression
instance ToTags Tsx.NonNullExpression
instance ToTags Tsx.Null
instance ToTags Tsx.Number
instance ToTags Tsx.Object
instance ToTags Tsx.ObjectPattern
instance ToTags Tsx.ObjectType
instance ToTags Tsx.OptionalParameter
instance ToTags Tsx.Pair
instance ToTags Tsx.ParenthesizedExpression
instance ToTags Tsx.ParenthesizedType
instance ToTags Tsx.PredefinedType
instance ToTags Tsx.Program
instance ToTags Tsx.PropertyIdentifier
instance ToTags Tsx.PropertySignature
instance ToTags Tsx.PublicFieldDefinition
instance ToTags Tsx.Readonly
instance ToTags Tsx.Regex
instance ToTags Tsx.RegexFlags
instance ToTags Tsx.RegexPattern
instance ToTags Tsx.RequiredParameter
instance ToTags Tsx.RestParameter
instance ToTags Tsx.ReturnStatement
instance ToTags Tsx.SequenceExpression
instance ToTags Tsx.ShorthandPropertyIdentifier
instance ToTags Tsx.SpreadElement
instance ToTags Tsx.Statement
instance ToTags Tsx.StatementBlock
instance ToTags Tsx.StatementIdentifier
instance ToTags Tsx.String
instance ToTags Tsx.SubscriptExpression
instance ToTags Tsx.Super
instance ToTags Tsx.SwitchBody
instance ToTags Tsx.SwitchCase
instance ToTags Tsx.SwitchDefault
instance ToTags Tsx.SwitchStatement
instance ToTags Tsx.TemplateString
instance ToTags Tsx.TemplateSubstitution
instance ToTags Tsx.TernaryExpression
instance ToTags Tsx.This
instance ToTags Tsx.ThrowStatement
instance ToTags Tsx.True
instance ToTags Tsx.TryStatement
instance ToTags Tsx.TupleType
instance ToTags Tsx.TypeAliasDeclaration
instance ToTags Tsx.TypeAnnotation
instance ToTags Tsx.TypeArguments
instance ToTags Tsx.TypeIdentifier
instance ToTags Tsx.TypeParameter
instance ToTags Tsx.TypeParameters
instance ToTags Tsx.TypePredicate
instance ToTags Tsx.TypeQuery
instance ToTags Tsx.UnaryExpression
instance ToTags Tsx.Undefined
instance ToTags Tsx.UnionType
instance ToTags Tsx.UpdateExpression
instance ToTags Tsx.VariableDeclaration
instance ToTags Tsx.VariableDeclarator
instance ToTags Tsx.WhileStatement
instance ToTags Tsx.WithStatement
instance ToTags Tsx.YieldExpression

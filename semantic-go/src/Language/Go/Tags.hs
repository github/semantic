{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Language.Go.Tags
( ToTags(..)
) where

import           AST.Element
import           AST.Token
import           AST.Traversable1
import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Text as Text
import qualified Language.Go.AST as Go
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

instance ToTags Go.FunctionDeclaration where
  tags t@Go.FunctionDeclaration
    { ann = loc@Loc { byteRange }
    , name = Go.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTags Go.MethodDeclaration where
  tags t@Go.MethodDeclaration
    { ann = loc@Loc { byteRange }
    , name = Go.FieldIdentifier { text }
    } = yieldTag text Method loc byteRange >> gtags t

instance ToTags Go.CallExpression where
  tags t@Go.CallExpression
    { ann = loc@Loc { byteRange }
    , function = Go.Expression expr
    } = match expr
      where
        match expr = case expr of
          Prj Go.SelectorExpression { field = Go.FieldIdentifier { text }}   -> yield text
          Prj Go.Identifier { text }                                         -> yield text
          Prj Go.CallExpression { function = Go.Expression e }               -> match e
          Prj Go.ParenthesizedExpression { extraChildren = Go.Expression e } -> match e
          _                                                                  -> gtags t
        yield name = yieldTag name Call loc byteRange >> gtags t

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

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> Kind -> Loc -> Range -> m ()
yieldTag name kind loc range = do
  src <- ask @Source
  Tags.yield (Tag name kind loc (Tags.firstLine src range) Nothing)


instance ToTags Go.ArgumentList
instance ToTags Go.ArrayType
instance ToTags Go.AssignmentStatement
instance ToTags Go.BinaryExpression
instance ToTags Go.BlankIdentifier
instance ToTags Go.Block
instance ToTags Go.BreakStatement
-- instance ToTags Go.CallExpression
instance ToTags Go.ChannelType
instance ToTags Go.CommunicationCase
instance ToTags Go.CompositeLiteral
instance ToTags Go.ConstDeclaration
instance ToTags Go.ConstSpec
instance ToTags Go.ContinueStatement
instance ToTags Go.DecStatement
instance ToTags Go.DefaultCase
instance ToTags Go.DeferStatement
instance ToTags Go.Dot
instance ToTags Go.Element
instance ToTags Go.EmptyStatement
instance ToTags Go.EscapeSequence
instance ToTags Go.Expression
instance ToTags Go.ExpressionCase
instance ToTags Go.ExpressionList
instance ToTags Go.ExpressionSwitchStatement
instance ToTags Go.FallthroughStatement
instance ToTags Go.False
instance ToTags Go.FieldDeclaration
instance ToTags Go.FieldDeclarationList
instance ToTags Go.FieldIdentifier
instance ToTags Go.FloatLiteral
instance ToTags Go.ForClause
instance ToTags Go.ForStatement
instance ToTags Go.FuncLiteral
-- instance ToTags Go.FunctionDeclaration
instance ToTags Go.FunctionType
instance ToTags Go.GoStatement
instance ToTags Go.GotoStatement
instance ToTags Go.Identifier
instance ToTags Go.IfStatement
instance ToTags Go.ImaginaryLiteral
instance ToTags Go.ImplicitLengthArrayType
instance ToTags Go.ImportDeclaration
instance ToTags Go.ImportSpec
instance ToTags Go.ImportSpecList
instance ToTags Go.IncStatement
instance ToTags Go.IndexExpression
instance ToTags Go.IntLiteral
instance ToTags Go.InterfaceType
instance ToTags Go.InterpretedStringLiteral
instance ToTags Go.KeyedElement
instance ToTags Go.LabelName
instance ToTags Go.LabeledStatement
instance ToTags Go.LiteralValue
instance ToTags Go.MapType
-- instance ToTags Go.MethodDeclaration
instance ToTags Go.MethodSpec
instance ToTags Go.MethodSpecList
instance ToTags Go.Nil
instance ToTags Go.PackageClause
instance ToTags Go.PackageIdentifier
instance ToTags Go.ParameterDeclaration
instance ToTags Go.ParameterList
instance ToTags Go.ParenthesizedExpression
instance ToTags Go.ParenthesizedType
instance ToTags Go.PointerType
instance ToTags Go.QualifiedType
instance ToTags Go.RangeClause
instance ToTags Go.RawStringLiteral
instance ToTags Go.ReceiveStatement
instance ToTags Go.ReturnStatement
instance ToTags Go.RuneLiteral
instance ToTags Go.SelectStatement
instance ToTags Go.SelectorExpression
instance ToTags Go.SendStatement
instance ToTags Go.ShortVarDeclaration
instance ToTags Go.SimpleStatement
instance ToTags Go.SimpleType
instance ToTags Go.SliceExpression
instance ToTags Go.SliceType
instance ToTags Go.SourceFile
instance ToTags Go.Statement
instance ToTags Go.StructType
instance ToTags Go.True
instance ToTags Go.Type
instance ToTags Go.TypeAlias
instance ToTags Go.TypeAssertionExpression
instance ToTags Go.TypeCase
instance ToTags Go.TypeConversionExpression
instance ToTags Go.TypeDeclaration
instance ToTags Go.TypeIdentifier
instance ToTags Go.TypeSpec
instance ToTags Go.TypeSwitchStatement
instance ToTags Go.UnaryExpression
instance ToTags Go.VarDeclaration
instance ToTags Go.VarSpec
instance ToTags Go.VariadicArgument
instance ToTags Go.VariadicParameterDeclaration

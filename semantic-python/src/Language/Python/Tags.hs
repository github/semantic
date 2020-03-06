{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Language.Python.Tags
( ToTags(..)
) where

import           AST.Element
import           AST.Token
import           AST.Traversable1
import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe (listToMaybe)
import           Data.Text as Text
import qualified Language.Python.AST as Py
import           Source.Loc
import           Source.Range
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

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

instance ToTags (Token sym n) where tags _ = pure ()

keywordFunctionCall
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Traversable1 ToTags t
     )
  => t Loc -> Loc -> Range -> Text -> m ()
keywordFunctionCall t loc range name = yieldTag name Function loc range Nothing >> gtags t

instance ToTags Py.String where
  tags Py.String { extraChildren } = for_ extraChildren $ \ x -> case x of
    Prj t@Py.Interpolation { } -> tags t
    _                          -> pure ()

instance ToTags Py.Interpolation where
  tags Py.Interpolation { extraChildren } = for_ extraChildren $ \ x -> case x of
    Prj (Py.Expression expr) -> tags expr
    _                        -> pure ()

instance ToTags Py.AssertStatement where
  tags t@Py.AssertStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "assert"

instance ToTags Py.Await where
  tags t@Py.Await { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "await"

instance ToTags Py.DeleteStatement where
  tags t@Py.DeleteStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "del"

instance ToTags Py.ExecStatement where
  tags t@Py.ExecStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "exec"

instance ToTags Py.GlobalStatement where
  tags t@Py.GlobalStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "global"

instance ToTags Py.NonlocalStatement where
  tags t@Py.NonlocalStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "nonlocal"

instance ToTags Py.PrintStatement where
  tags t@Py.PrintStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "print"

instance ToTags Py.FunctionDefinition where
  tags t@Py.FunctionDefinition
    { ann = loc@Loc { byteRange = Range { start } }
    , name = Py.Identifier { text = name }
    , body = Py.Block { ann = Loc Range { start = end } _, extraChildren }
    } = do
      src <- ask @Source
      let docs = listToMaybe extraChildren >>= docComment src
      yieldTag name Function loc (Range start end) docs >> gtags t

instance ToTags Py.ClassDefinition where
  tags t@Py.ClassDefinition
    { ann = loc@Loc { byteRange = Range { start } }
    , name = Py.Identifier { text = name }
    , body = Py.Block { ann = Loc Range { start = end } _, extraChildren }
    } = do
      src <- ask @Source
      let docs = listToMaybe extraChildren >>= docComment src
      yieldTag name Class loc (Range start end) docs >> gtags t

instance ToTags Py.Call where
  tags t@Py.Call
    { ann = loc@Loc { byteRange = range }
    , function = Py.PrimaryExpression expr
    } = match expr
    where
      match expr = case expr of
        (Prj Py.Attribute { attribute = Py.Identifier _ name })                                       -> yield name
        (Prj (Py.Identifier _ name))                                                                  -> yield name
        (Prj Py.Call { function = Py.PrimaryExpression expr' })                                       -> match expr' -- Nested call expression like this in Python represent creating an instance of a class and calling it: e.g. AClass()()
        (Prj (Py.ParenthesizedExpression _ (Prj (Py.Expression (Prj (Py.PrimaryExpression expr')))))) -> match expr' -- Parenthesized expressions
        _                                                                                             -> gtags t
      yield name = yieldTag name Call loc range Nothing >> gtags t

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> Kind -> Loc -> Range -> Maybe Text -> m ()
yieldTag name kind loc range docs = do
  src <- ask @Source
  Tags.yield (Tag name kind loc (Tags.firstLine src range) docs)

docComment :: Source -> (Py.CompoundStatement :+: Py.SimpleStatement) Loc -> Maybe Text
docComment src (R1 (Py.SimpleStatement (Prj Py.ExpressionStatement { extraChildren = L1 (Prj (Py.Expression (Prj (Py.PrimaryExpression (Prj Py.String { ann }))))) :|_ }))) = Just (toText (slice src (byteRange ann)))
docComment _ _ = Nothing


gtags
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Traversable1 ToTags t
     )
  => t Loc
  -> m ()
gtags = traverse1_ @ToTags (const (pure ())) tags

instance ToTags Py.AliasedImport
instance ToTags Py.ArgumentList
-- instance ToTags Py.AssertStatement
instance ToTags Py.Assignment
instance ToTags Py.Attribute
instance ToTags Py.AugmentedAssignment
-- instance ToTags Py.Await
instance ToTags Py.BinaryOperator
instance ToTags Py.Block
instance ToTags Py.BooleanOperator
instance ToTags Py.BreakStatement
-- instance ToTags Py.Call
instance ToTags Py.Chevron
-- instance ToTags Py.ClassDefinition
instance ToTags Py.ComparisonOperator
instance ToTags Py.CompoundStatement
instance ToTags Py.ConcatenatedString
instance ToTags Py.ConditionalExpression
instance ToTags Py.ContinueStatement
instance ToTags Py.DecoratedDefinition
instance ToTags Py.Decorator
instance ToTags Py.DefaultParameter
-- instance ToTags Py.DeleteStatement
instance ToTags Py.Dictionary
instance ToTags Py.DictionaryComprehension
instance ToTags Py.DictionarySplat
instance ToTags Py.DottedName
instance ToTags Py.ElifClause
instance ToTags Py.Ellipsis
instance ToTags Py.ElseClause
instance ToTags Py.EscapeSequence
instance ToTags Py.ExceptClause
-- instance ToTags Py.ExecStatement
instance ToTags Py.Expression
instance ToTags Py.ExpressionList
instance ToTags Py.ExpressionStatement
instance ToTags Py.False
instance ToTags Py.FinallyClause
instance ToTags Py.Float
instance ToTags Py.ForInClause
instance ToTags Py.ForStatement
instance ToTags Py.FormatExpression
instance ToTags Py.FormatSpecifier
-- instance ToTags Py.FunctionDefinition
instance ToTags Py.FutureImportStatement
instance ToTags Py.GeneratorExpression
-- instance ToTags Py.GlobalStatement
instance ToTags Py.Identifier
instance ToTags Py.IfClause
instance ToTags Py.IfStatement
instance ToTags Py.ImportFromStatement
instance ToTags Py.ImportPrefix
instance ToTags Py.ImportStatement
instance ToTags Py.Integer
-- instance ToTags Py.Interpolation
instance ToTags Py.KeywordArgument
instance ToTags Py.Lambda
instance ToTags Py.LambdaParameters
instance ToTags Py.List
instance ToTags Py.ListComprehension
instance ToTags Py.ListSplat
instance ToTags Py.Module
instance ToTags Py.NamedExpression
instance ToTags Py.None
-- instance ToTags Py.NonlocalStatement
instance ToTags Py.NotOperator
instance ToTags Py.Pair
instance ToTags Py.Parameter
instance ToTags Py.Parameters
instance ToTags Py.ParenthesizedExpression
instance ToTags Py.PassStatement
instance ToTags Py.PrimaryExpression
-- instance ToTags Py.PrintStatement
instance ToTags Py.RaiseStatement
instance ToTags Py.RelativeImport
instance ToTags Py.ReturnStatement
instance ToTags Py.Set
instance ToTags Py.SetComprehension
instance ToTags Py.SimpleStatement
instance ToTags Py.Slice
instance ToTags Py.Subscript
instance ToTags Py.True
instance ToTags Py.TryStatement
instance ToTags Py.Tuple
instance ToTags Py.Type
instance ToTags Py.TypeConversion
instance ToTags Py.TypedDefaultParameter
instance ToTags Py.TypedParameter
instance ToTags Py.UnaryOperator
instance ToTags Py.Variables
instance ToTags Py.WhileStatement
instance ToTags Py.WildcardImport
instance ToTags Py.WithItem
instance ToTags Py.WithStatement
instance ToTags Py.Yield

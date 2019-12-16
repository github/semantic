{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Ruby.Tags
( ToTags(..)
) where

import           AST.Element
import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Maybe (listToMaybe)
import           Data.Monoid (Ap(..))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text as Text
import           GHC.Generics
import           Source.Loc
import           Source.Range
import           Source.Source as Source
import           Tags.Tag
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Ruby.AST as Rb

class ToTags t where
  tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       )
    => t Loc
    -> m ()

instance (ToTagsBy strategy t, strategy ~ ToTagsInstance t) => ToTags t where
  tags = tags' @strategy


class ToTagsBy (strategy :: Strategy) t where
  tags'
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       )
    => t Loc
    -> m ()


data Strategy = Generic | Custom

type family ToTagsInstance t :: Strategy where
  ToTagsInstance (_ :+: _)             = 'Custom
  ToTagsInstance Rb.Class              = 'Custom
  ToTagsInstance Rb.Module             = 'Custom
  ToTagsInstance Rb.Method             = 'Custom
  ToTagsInstance Rb.SingletonMethod    = 'Custom

  ToTagsInstance Rb.Call               = 'Custom
  ToTagsInstance Rb.Lhs                = 'Custom
  ToTagsInstance Rb.MethodCall         = 'Custom
  ToTagsInstance _                     = 'Generic


instance (ToTags l, ToTags r) => ToTagsBy 'Custom (l :+: r) where
  tags' (L1 l) = tags l
  tags' (R1 r) = tags r

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> Kind -> Loc -> Range -> m ()
yieldTag name kind loc range = do
  src <- ask @Source
  let sliced = slice src range
  Tags.yield (Tag name kind loc (Tags.firstLine sliced) Nothing)

instance ToTagsBy 'Custom Rb.Class  where
  tags' t@Rb.Class
    { ann = loc@Loc { byteRange = range }
    , name = expr
    } = case expr of
      Prj Rb.Constant { text = name } -> yield name
      Prj Rb.ScopeResolution { name = Prj Rb.Constant { text = name } } -> yield name
      Prj Rb.ScopeResolution { name = Prj Rb.Identifier { text = name } } -> yield name
      _ -> gtags t
    where
      yield name = yieldTag name Class loc range >> gtags t

instance ToTagsBy 'Custom Rb.Module  where
  tags' t@Rb.Module
    { ann = loc@Loc { byteRange = range }
    , name = expr
    } = case expr of
      Prj Rb.Constant { text = name } -> yield name
      Prj Rb.ScopeResolution { name = Prj Rb.Constant { text = name } } -> yield name
      Prj Rb.ScopeResolution { name = Prj Rb.Identifier { text = name } } -> yield name
      _ -> gtags t
    where
      yield name = yieldTag name Module loc range >> gtags t

yieldMethodNameTag t loc range expr = case expr of
  Prj Rb.Identifier { text = name } -> yield name
  Prj Rb.Constant { text = name } -> yield name
  Prj Rb.ClassVariable { text = name } -> yield name
  Prj Rb.Operator { text = name } -> yield name
  Prj Rb.GlobalVariable { text = name } -> yield name
  Prj Rb.InstanceVariable { text = name } -> yield name
  Prj Rb.Setter { extraChildren = Rb.Identifier { text = name } } -> yield name
  -- TODO: Should we report symbol method names as tags?
  -- Prj Rb.Symbol { extraChildren = [Prj Rb.EscapeSequence { text = name }] } -> yield name
  _ -> gtags t
  where
    yield name = yieldTag name Function loc range >> gtags t

instance ToTagsBy 'Custom Rb.Method  where
  tags' t@Rb.Method
    { ann = loc@Loc { byteRange = range }
    , name = Rb.MethodName expr
    } = yieldMethodNameTag t loc range expr

instance ToTagsBy 'Custom Rb.SingletonMethod  where
  tags' t@Rb.SingletonMethod
    { ann = loc@Loc { byteRange = range }
    , name = Rb.MethodName expr
    } = yieldMethodNameTag t loc range expr

instance ToTagsBy 'Custom Rb.Call where
  tags' t@Rb.Call
    { ann = loc@Loc { byteRange = range }
    , method = expr
    } = do
      case expr of
        Prj Rb.Identifier { text = name } -> yield name
        Prj Rb.Constant { text = name } -> yield name
        Prj Rb.Operator { text = name } -> yield name
        _ -> gtags t
    where
      yield name = yieldTag name Call loc range >> gtags t

instance ToTagsBy 'Custom Rb.Lhs where
  tags' t@(Rb.Lhs (Prj (Rb.Variable expr)))
    = case expr of
      Prj Rb.Identifier { ann = loc@Loc { byteRange = range }, text = name } -> yieldTag name Call loc range >> gtags t
      Prj Rb.Constant { ann = loc@Loc { byteRange = range }, text = name } -> yieldTag name Call loc range >> gtags t
      _ -> gtags t
  tags' t = gtags t

instance ToTagsBy 'Custom Rb.MethodCall where
  tags' t@Rb.MethodCall
    { ann = loc@Loc { byteRange = range }
    , method = expr
    } = case expr of
      Prj (Rb.Variable (Prj Rb.Identifier { text = name })) -> yield name
      Prj (Rb.Variable (Prj Rb.Constant { text = name })) -> yield name
      Prj (Rb.Variable (Prj Rb.GlobalVariable { text = name })) -> yield name
      Prj (Rb.Variable (Prj Rb.ClassVariable { text = name })) -> yield name
      Prj (Rb.Variable (Prj Rb.InstanceVariable { text = name })) -> yield name
      _ -> gtags t
    where
      yield name = yieldTag name Call loc range >> gtags t

-- docComment :: Source -> (Rb.CompoundStatement :+: Rb.SimpleStatement) Loc -> Maybe Text
-- docComment src (R1 (Rb.SimpleStatement (Prj Rb.ExpressionStatement { extraChildren = L1 (Prj (Rb.Expression (Prj (Rb.PrimaryExpression (Prj Rb.String { ann }))))) :|_ }))) = Just (toText (slice src (byteRange ann)))
-- docComment _ _ = Nothing

gtags
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Generic1 t
     , Tags.GFoldable1 ToTags (Rep1 t)
     )
  => t Loc
  -> m ()
gtags = getAp . Tags.gfoldMap1 @ToTags (Ap . tags) . from1

instance (Generic1 t, Tags.GFoldable1 ToTags (Rep1 t)) => ToTagsBy 'Generic t where
  tags' = gtags

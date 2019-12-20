{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, PartialTypeSignatures, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
module Language.TypeScript.Tags
( ToTags(..)
) where

import           AST.Element
import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Foldable
import           Data.Monoid (Ap (..))
import           Data.Text as Text
import           GHC.Generics
import           Source.Loc
import           Source.Source as Source
import           Tags.Tag
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.TypeScript.AST as Ts

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
  ToTagsInstance (_ :+: _)              = 'Custom
  ToTagsInstance Ts.CallExpression      = 'Custom
  ToTagsInstance Ts.ClassDeclaration    = 'Custom
  ToTagsInstance Ts.Function            = 'Custom
  ToTagsInstance Ts.FunctionDeclaration = 'Custom
  ToTagsInstance Ts.FunctionSignature   = 'Custom
  ToTagsInstance Ts.MethodDefinition    = 'Custom
  ToTagsInstance _                      = 'Generic

instance ToTagsBy 'Custom Ts.Function where
  tags' t@Ts.Function
    { ann = loc@Loc { byteRange }
    , name = Just Ts.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t
  tags' t = gtags t

instance ToTagsBy 'Custom Ts.FunctionSignature where
  tags' t@Ts.FunctionSignature
    { ann = loc@Loc { byteRange }
    , name = Ts.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTagsBy 'Custom Ts.FunctionDeclaration where
  tags' t@Ts.FunctionDeclaration
    { ann = loc@Loc { byteRange }
    , name = Ts.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTagsBy 'Custom Ts.MethodDefinition where
  tags' t@Ts.MethodDefinition
    { ann = loc@Loc { byteRange }
    , name
    } = case name of
      Prj Ts.PropertyIdentifier { text } -> yield text
      -- TODO: There are more here
      _ -> gtags t
      where
        yield name = yieldTag name Call loc byteRange >> gtags t

instance ToTagsBy 'Custom Ts.ClassDeclaration where
  tags' t@Ts.ClassDeclaration
    { ann = loc@Loc { byteRange }
    , name = Ts.TypeIdentifier { text }
    } = yieldTag text Class loc byteRange >> gtags t

instance ToTagsBy 'Custom Ts.CallExpression where
  tags' t@Ts.CallExpression
    { ann = loc@Loc { byteRange }
    , function = Ts.Expression expr
    } = match expr
    where
      match expr = case expr of
        Prj Ts.Identifier { text } -> yield text
        Prj Ts.NewExpression { constructor = Prj Ts.Identifier { text } } -> yield text
        Prj Ts.CallExpression { function = Ts.Expression expr } -> match expr
        Prj Ts.MemberExpression { property = Ts.PropertyIdentifier { text } } -> yield text
        Prj Ts.Function { name = Just Ts.Identifier { text }} -> yield text
        Prj Ts.ParenthesizedExpression { extraChildren } -> for_ extraChildren $ \ x -> case x of
          Prj (Ts.Expression expr) -> match expr
          _ -> tags x
        _ -> gtags t
      yield name = yieldTag name Call loc byteRange >> gtags t

instance (ToTags l, ToTags r) => ToTagsBy 'Custom (l :+: r) where
  tags' (L1 l) = tags l
  tags' (R1 r) = tags r

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

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> Kind -> Loc -> Range -> m ()
yieldTag name kind loc range = do
  src <- ask @Source
  let sliced = slice src range
  Tags.yield (Tag name kind loc (Tags.firstLine sliced) Nothing)

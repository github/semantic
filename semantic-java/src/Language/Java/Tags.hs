{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
-- FIXME: it would be really nice if we didn’t need to do this
{-# OPTIONS_GHC -freduction-depth=0 #-}
module Language.Java.Tags
( ToTags(..)
) where

import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Foldable (traverse_)
import           Data.Monoid (Ap(..))
import           Data.Text as Text
import           GHC.Generics
import           Source.Loc
import           Source.Range
import           Source.Source as Source
import           Tags.Tag
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Java.AST as Java

class ToTags t where
  tags
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Writer Tags.Tags) sig
       )
    => t Loc
    -> m ()

instance (ToTagsBy strategy t, strategy ~ ToTagsInstance t) => ToTags t where
  tags = tags' @strategy


class ToTagsBy (strategy :: Strategy) t where
  tags'
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Writer Tags.Tags) sig
       )
    => t Loc
    -> m ()


data Strategy = Generic | Custom

type family ToTagsInstance t :: Strategy where
  ToTagsInstance (_ :+: _)              = 'Custom
  ToTagsInstance Java.Program           = 'Custom
  ToTagsInstance Java.MethodDeclaration = 'Custom
  ToTagsInstance _                      = 'Generic


instance (ToTags l, ToTags r) => ToTagsBy 'Custom (l :+: r) where
  tags' (L1 l) = tags l
  tags' (R1 r) = tags r

instance ToTagsBy 'Custom Java.Program where
  tags' Java.Program { extraChildren } = traverse_ tags extraChildren

instance ToTagsBy 'Custom Java.MethodDeclaration where
  tags' Java.MethodDeclaration
    { ann = Loc range span
    , name = Java.Identifier { bytes = name }
    , dimensions
    , extraChildren
    , typeParameters
    , parameters
    , type'
    , body
    } = do
      src <- ask @Source
      let sliced = slice src range
            { end = case body of
              Just Java.Block { ann = Loc Range { end } _ } -> end
              Nothing                                       -> end range
            }
      Tags.yield (Tag name Function span (firstLine sliced) Nothing)
      traverse_ tags typeParameters
      tags parameters
      tags type'
      traverse_ tags dimensions
      traverse_ tags extraChildren
      traverse_ tags body

firstLine :: Source -> Text
firstLine = Text.takeWhile (/= '\n') . toText . Source.take 180


gtags
  :: ( Carrier sig m
     , Member (Reader Source) sig
     , Member (Writer Tags.Tags) sig
     , Generic1 t
     , Tags.GFoldable1 ToTags (Rep1 t)
     )
  => t Loc
  -> m ()
gtags = getAp . Tags.gfoldMap1 @ToTags (Ap . tags) . from1

instance (Generic1 t, Tags.GFoldable1 ToTags (Rep1 t)) => ToTagsBy 'Generic t where
  tags' = gtags

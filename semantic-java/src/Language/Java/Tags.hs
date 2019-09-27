{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Java.Tags
( ToTags(..)
) where

import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Foldable (traverse_)
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
  ToTagsInstance _                      = 'Generic


instance (ToTags l, ToTags r) => ToTagsBy 'Custom (l :+: r) where
  tags' (L1 l) = tags l
  tags' (R1 r) = tags r

firstLine :: Source -> Text
firstLine = Text.takeWhile (/= '\n') . toText . Source.take 180


instance (Generic1 t, Tags.GFoldable1 ToTags (Rep1 t)) => ToTagsBy 'Generic t where
  tags' = getAp . Tags.gfoldMap1 @ToTags (Ap . tags) . from1

{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Java.Tags
( ToTags(..)
) where

import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Monoid (Ap(..))
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
  ToTagsInstance Java.MethodDeclaration = 'Custom
  ToTagsInstance Java.MethodInvocation  = 'Custom
  ToTagsInstance Java.ClassDeclaration  = 'Custom
  ToTagsInstance _                      = 'Generic


instance (ToTags l, ToTags r) => ToTagsBy 'Custom (l :+: r) where
  tags' (L1 l) = tags l
  tags' (R1 r) = tags r

instance ToTagsBy 'Custom Java.MethodDeclaration where
  tags' t@Java.MethodDeclaration
    { ann = loc@Loc { byteRange = range }
    , name = Java.Identifier { text = name }
    , body
    } = do
      src <- ask @Source
      let sliced = slice src range
            { end = case body of
              Just Java.Block { ann = Loc Range { end } _ } -> end
              Nothing                                       -> end range
            }
      Tags.yield (Tag name Method loc (Tags.firstLine sliced) Nothing)
      gtags t

instance ToTagsBy 'Custom Java.ClassDeclaration where
  tags' t@Java.ClassDeclaration
    { ann = loc@Loc { byteRange = Range { start } }
    , name = Java.Identifier { text = name }
    , body = Java.ClassBody { ann = Loc Range { start = end } _ }
    } = do
      src <- ask @Source
      let sliced = slice src (Range start end)
      Tags.yield (Tag name Class loc (Tags.firstLine sliced) Nothing)
      gtags t

instance ToTagsBy 'Custom Java.MethodInvocation where
  tags' t@Java.MethodInvocation
    { ann = loc@Loc { byteRange = range }
    , name = Java.Identifier { text = name }
    } = do
      src <- ask @Source
      let sliced = slice src range
      Tags.yield (Tag name Call loc (Tags.firstLine sliced) Nothing)
      gtags t


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

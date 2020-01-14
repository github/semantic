{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Language.Go.Tags
( ToTags(..)
) where

import           AST.Element
import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Text as Text
import           GHC.Generics
import           Source.Loc
import           Source.Source as Source
import           Tags.Tag
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Go.AST as Go

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
  ToTagsInstance Go.FunctionDeclaration = 'Custom
  ToTagsInstance Go.MethodDeclaration   = 'Custom
  ToTagsInstance Go.CallExpression      = 'Custom
  ToTagsInstance _                      = 'Generic

instance ToTagsBy 'Custom Go.FunctionDeclaration where
  tags' t@Go.FunctionDeclaration
    { ann = loc@Loc { byteRange }
    , name = Go.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTagsBy 'Custom Go.MethodDeclaration where
  tags' t@Go.MethodDeclaration
    { ann = loc@Loc { byteRange }
    , name = Go.FieldIdentifier { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTagsBy 'Custom Go.CallExpression where
  tags' t@Go.CallExpression
    { ann = loc@Loc { byteRange }
    , function = Go.Expression expr
    } = match expr
      where
        match expr = case expr of
          Prj Go.SelectorExpression { field = Go.FieldIdentifier { text }} -> yield text
          Prj Go.Identifier { text } -> yield text
          Prj Go.CallExpression { function = Go.Expression e } -> match e
          Prj Go.ParenthesizedExpression { extraChildren = Go.Expression e } -> match e
          _ -> gtags t
        yield name = yieldTag name Call loc byteRange >> gtags t

instance (ToTags l, ToTags r) => ToTagsBy 'Custom (l :+: r) where
  tags' (L1 l) = tags l
  tags' (R1 r) = tags r

gtags
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Generic1 t
     , Tags.GTraversable1 ToTags (Rep1 t)
     )
  => t Loc
  -> m ()
gtags = Tags.traverse1_ @ToTags (const (pure ())) tags . Tags.Generics

instance (Generic1 t, Tags.GTraversable1 ToTags (Rep1 t)) => ToTagsBy 'Generic t where
  tags' = gtags

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> Kind -> Loc -> Range -> m ()
yieldTag name kind loc range = do
  src <- ask @Source
  let sliced = slice src range
  Tags.yield (Tag name kind loc (Tags.firstLine sliced) Nothing)

{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

{-# OPTIONS_GHC -freduction-depth=0 #-}
module Language.TSX.Tags
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
import qualified TreeSitter.TSX.AST as Tsx

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
  ToTagsInstance Tsx.CallExpression      = 'Custom
  ToTagsInstance Tsx.Class               = 'Custom
  ToTagsInstance Tsx.ClassDeclaration    = 'Custom
  ToTagsInstance Tsx.Function            = 'Custom
  ToTagsInstance Tsx.FunctionDeclaration = 'Custom
  ToTagsInstance Tsx.FunctionSignature   = 'Custom
  ToTagsInstance Tsx.MethodDefinition    = 'Custom
  ToTagsInstance _                      = 'Generic

instance ToTagsBy 'Custom Tsx.Function where
  tags' t@Tsx.Function
    { ann = loc@Loc { byteRange }
    , name = Just Tsx.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t
  tags' t = gtags t

instance ToTagsBy 'Custom Tsx.FunctionSignature where
  tags' t@Tsx.FunctionSignature
    { ann = loc@Loc { byteRange }
    , name = Tsx.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTagsBy 'Custom Tsx.FunctionDeclaration where
  tags' t@Tsx.FunctionDeclaration
    { ann = loc@Loc { byteRange }
    , name = Tsx.Identifier { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTagsBy 'Custom Tsx.MethodDefinition where
  tags' t@Tsx.MethodDefinition
    { ann = loc@Loc { byteRange }
    , name
    } = case name of
      Prj Tsx.PropertyIdentifier { text } -> yield text
      -- TODO: There are more here
      _ -> gtags t
      where
        yield name = yieldTag name Call loc byteRange >> gtags t

instance ToTagsBy 'Custom Tsx.ClassDeclaration where
  tags' t@Tsx.ClassDeclaration
    { ann = loc@Loc { byteRange }
    , name = Tsx.TypeIdentifier { text }
    } = yieldTag text Class loc byteRange >> gtags t

instance ToTagsBy 'Custom Tsx.CallExpression where
  tags' t@Tsx.CallExpression
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
          _ -> tags x
        _ -> gtags t
      yield name = yieldTag name Call loc byteRange >> gtags t

instance ToTagsBy 'Custom Tsx.Class where
  tags' t@Tsx.Class
    { ann = loc@Loc { byteRange }
    , name = Just Tsx.TypeIdentifier { text }
    } = yieldTag text Class loc byteRange >> gtags t
  tags' t = gtags t

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
  let sliced = slice src range
  Tags.yield (Tag name kind loc (Tags.firstLine sliced) Nothing)

-- | Semantic functionality for Java programs.
{-# OPTIONS_GHC -freduction-depth=0 #-}
module Language.Java
( Term(..)
) where

import qualified Language.Java.Tags as JavaTags
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Java.AST as Java
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: Java.Program a }

instance TS.Unmarshal Term where
  unmarshalNode node = Term <$> TS.unmarshalNode node

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . JavaTags.tags . getTerm

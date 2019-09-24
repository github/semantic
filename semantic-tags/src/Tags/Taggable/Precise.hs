{-# LANGUAGE FlexibleContexts #-}
module Tags.Taggable.Precise
( runTagging
, Tags
, ToTags(..)
, yield
) where

import Control.Effect.Reader
import Control.Effect.Writer
import Data.Monoid (Endo(..))
import Source.Loc
import Source.Source
import Tags.Tag

runTagging :: ToTags t => Source -> t Loc -> [Tag]
runTagging source
  = ($ [])
  . appEndo
  . run
  . execWriter
  . runReader source
  . tags

type Tags = Endo [Tag]

class ToTags t where
  tags
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Writer Tags) sig
       )
    => t Loc
    -> m ()


yield :: (Carrier sig m, Member (Writer Tags) sig) => Tag -> m ()
yield = tell . Endo . (:)

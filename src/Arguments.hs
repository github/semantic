{-# LANGUAGE GADTs, DuplicateRecordFields, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Arguments where

import Data.Maybe
import Language
import Prologue
import Renderer

data DiffMode = DiffStdin | DiffPaths (FilePath, Maybe Language) (FilePath, Maybe Language)
  deriving Show

data DiffArguments where
  DiffArguments :: (Monoid output, StringConv output ByteString) =>
    { diffRenderer :: DiffRenderer output
    , diffMode :: DiffMode
    } -> DiffArguments

deriving instance Show DiffArguments


data ParseMode = ParseStdin | ParsePaths [(FilePath, Maybe Language)]
  deriving Show

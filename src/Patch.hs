module Patch where

data Patch a = Patch { old :: Maybe a, new :: Maybe a }

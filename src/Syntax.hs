module Syntax where

import Data.Map

data Syntax a f =
    Leaf a
    | Indexed [f]
    | Fixed [f]
    | Keyed (Map String f)
    deriving (Functor, Show, Eq)

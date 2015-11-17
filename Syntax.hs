module Syntax where

{-# LANGUAGE DeriveFunctor #-}

import Data.Map

data Syntax a f =
    Leaf a
    | Indexed [f]
    | Fixed [f]
    | Keyed (Map String f)
    deriving (Functor, Show, Eq)

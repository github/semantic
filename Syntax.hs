module Syntax where

{-# LANGUAGE DeriveFunctor #-}

import Data.Map

data Syntax f a =
    Leaf a
    | Indexed [f]
    | Fixed [f]
    | Keyed (Map String f)
    deriving (Functor, Show, Eq)

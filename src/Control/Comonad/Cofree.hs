module Control.Comonad.Cofree where

data Cofree functor annotation = annotation :< functor

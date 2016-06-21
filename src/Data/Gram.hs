module Data.Gram where

data Gram label = Gram { stem :: [label], base :: [label] }

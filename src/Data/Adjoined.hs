module Data.Adjoined where

newtype Adjoined a = Adjoined { unAdjoined :: [a] }

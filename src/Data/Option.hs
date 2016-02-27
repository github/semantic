module Data.Option where

newtype Option a = Option { getOption :: Maybe a }

option :: b -> (a -> b) -> Option a -> b
option b f = maybe b f . getOption

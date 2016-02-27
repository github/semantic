module Data.Option where

newtype Option a = Option { getOption :: Maybe a }

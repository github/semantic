module Data.Bifunctor.Join where

newtype Join p a = { runJoin :: p a a }

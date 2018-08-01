{-# LANGUAGE GADTs, KindSignatures, LambdaCase, TypeOperators #-}
module Semantic.REPL () where

data REPL (m :: * -> *) result where
  Prompt :: REPL m String
  Output :: String -> REPL m ()

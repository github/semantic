{-# LANGUAGE GADTs, KindSignatures, LambdaCase, TypeOperators #-}
module Semantic.REPL () where

data REPL (m :: * -> *) result where
  Prompt :: REPL m String
  Output :: String -> REPL m ()

prompt :: (Effectful m, Member REPL effects) => m effects String
prompt = send Prompt

output :: (Effectful m, Member REPL effects) => String -> m effects ()
output s = send (Output s)

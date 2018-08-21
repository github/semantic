module Language.Ruby.Translate
  ( translatingRuby
  ) where

import Data.Machine
import Data.Semigroup (stimesMonoid)
import Data.Reprinting.Splice
import Data.Reprinting.Token
import Data.Sequence (Seq)

translatingRuby :: Monad m => ProcessT m Splice Splice
translatingRuby = flattened <~ auto step where

step :: Splice -> Seq Splice
step s@(Unhandled el cs) = case (el, cs) of
  (TOpen,  TMethod:_) -> emit "def" <> layout Space
  (TClose, TMethod:_) -> emit "end"

  (TOpen,  TParams:TMethod:_) -> emit "("
  (TSep,   TParams:TMethod:_) -> emit "," <> layout Space
  (TClose, TParams:TMethod:_) -> emit ")"

  (TOpen,  Imperative:xs) -> indented (n xs)
  (TSep,   Imperative:xs) -> indented (n xs)
  (TClose, Imperative:_)  -> layout HardWrap

  -- (TOpen, TComment:_) -> layout HardWrap
  -- (TSep, TComment:_) -> layout HardWrap

  _ -> pure s

  where
    indented times = stimesMonoid times (layouts [HardWrap, Indent])
    emit = splice el cs
    n = length . filter (== Imperative)

step x = pure x

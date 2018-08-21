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
  (Open,  (Declaration DeclMethod):_) -> emit "def" <> directive Space
  (Close, (Declaration DeclMethod):_) -> emit "end"
  (Open,  (List:(Declaration DeclMethod):_)) -> emit "("
  (Close, (List:(Declaration DeclMethod):_)) -> emit ")"

  (Open,      Imperative:xs) -> stimesMonoid (n xs) (directives [HardWrap, Indent])
  (Separator, Imperative:xs) -> stimesMonoid (n xs) (directives [HardWrap, Indent])
  (Close,     Imperative:_)  -> directive HardWrap

  _ -> pure s

  where
    emit = splice el cs
    n = length . filter (== Imperative)

step x = pure x

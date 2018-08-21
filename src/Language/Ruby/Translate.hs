module Language.Ruby.Translate
  ( translatingRuby
  ) where

import Data.Machine
import Data.Semigroup (stimes)
import Data.Reprinting.Splice
import Data.Reprinting.Token
import Data.Sequence (Seq)

translatingRuby :: Monad m => ProcessT m Splice Splice
translatingRuby = flattened <~ auto step where

step :: Splice -> Seq Splice
step s@(Unhandled el cs) = case (el, cs) of
  (TOpen,  TMethod:_)  -> emit "def" <> layout Space
  (TClose, TMethod:xs) -> endContext (depth xs) <> emit "end"

  (TOpen,  TParams:TMethod:_) -> emit "("
  (TSep,   TParams:TMethod:_) -> emit "," <> layout Space
  (TClose, TParams:TMethod:_) -> emit ")"

  (TOpen,  Imperative:[]) -> mempty
  (TOpen,  Imperative:xs) -> layout HardWrap <> indent (depth xs)
  (TSep,   Imperative:xs) -> layout HardWrap <> indent (depth xs)
  (TClose, Imperative:[]) -> layout HardWrap
  (TClose, Imperative:xs) -> indent (pred (depth xs))

  _ -> pure s

  where
    endContext times = layout HardWrap <> indent (pred times)
    emit = splice el cs

step x = pure x

-- | Depth of imperative scope.
depth :: [Context] -> Int
depth = length . filter (== Imperative)

-- | Indent n times.
indent :: Integral b => b -> Seq Splice
indent times
  | times > 0 = stimes times (layout Indent)
  | otherwise = mempty

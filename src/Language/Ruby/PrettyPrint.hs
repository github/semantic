{-# LANGUAGE Rank2Types #-}

module Language.Ruby.PrettyPrint ( printingRuby ) where

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Effect
import Control.Monad.Effect.Exception (Exc, throwError)
import Data.Machine
import Data.Reprinting.Errors
import Data.Reprinting.Splice
import Data.Reprinting.Token as Token

-- | Print Ruby syntax.
printingRuby :: (Member (Exc TranslationError) effs) => ProcessT (Eff effs) Fragment Splice
printingRuby = repeatedly (await >>= step)

step :: (Member (Exc TranslationError) effs) => Fragment -> PlanT k Splice (Eff effs) ()
step (Verbatim txt) = emit txt
step (New _ _ txt)  = emit txt
step (Defer el cs)  = case (el, cs) of
  (TOpen,  TMethod:_)  -> emit "def" *> space
  (TClose, TMethod:xs) -> endContext (depth xs) *> emit "end"

  -- TODO: do..end vs {..} should be configurable.
  (TOpen,  TFunction:_)         -> space *> emit "do" *> space
  (TOpen,  TParams:TFunction:_) -> emit "|"
  (TClose, TParams:TFunction:_) -> emit "|"
  (TClose, TFunction:xs)        -> endContext (depth xs) *> emit "end"

  -- TODO: Parens for calls are a style choice, make configurable.
  (TOpen,  TParams:_) -> emit "("
  (TSep,   TParams:_) -> emit "," *> space
  (TClose, TParams:_) -> emit ")"

  (TOpen,  TInfixL _ p:xs)       -> emitIf (p < prec xs) "("
  (TSym,   TInfixL Add _:_)      -> space *> emit "+" *> space
  (TSym,   TInfixL Multiply _:_) -> space *> emit "*" *> space
  (TSym,   TInfixL Subtract _:_) -> space *> emit "-" *> space
  (TClose, TInfixL _ p:xs)       -> emitIf (p < prec xs) ")"

  (TOpen,  [Imperative])  -> pure ()
  (TOpen,  Imperative:xs) -> layout HardWrap *> indent (depth xs)
  (TSep,   Imperative:xs) -> layout HardWrap *> indent (depth xs)
  (TClose, [Imperative])  -> layout HardWrap
  (TClose, Imperative:xs) -> indent (pred (depth xs))

  (TSep, TCall:_) -> emit "."

  _ -> lift (throwError (NoTranslation el cs))

  where
    emitIf predicate txt = when predicate (emit txt)
    endContext times = layout HardWrap *> indent (pred times)

prec :: [Context] -> Int
prec cs = case filter isInfix cs of
  (TInfixL _ n:_) -> n
  _ -> 0
  where isInfix (TInfixL _ _) = True
        isInfix _             = False

-- | Depth of imperative scope.
depth :: [Context] -> Int
depth = length . filter (== Imperative)

-- | Indent n times.
indent :: Int -> Plan k Splice ()
indent times
  | times > 0 = replicateM_ times (layout (Indent 2 Spaces))
  | otherwise = pure ()

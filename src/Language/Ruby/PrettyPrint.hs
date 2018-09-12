{-# LANGUAGE Rank2Types #-}

module Language.Ruby.PrettyPrint ( printingRuby ) where

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
  (TClose, TMethod:xs) -> endContext (imperativeDepth xs) *> emit "end"

  -- TODO: do..end vs {..} should be configurable.
  (TOpen,  TFunction:_)         -> space *> emit "do" *> space
  (TOpen,  TParams:TFunction:_) -> emit "|"
  (TClose, TParams:TFunction:_) -> emit "|"
  (TClose, TFunction:xs)        -> endContext (imperativeDepth xs) *> emit "end"

  -- TODO: Parens for calls are a style choice, make configurable.
  (TOpen,  TParams:_) -> emit "("
  (TSep,   TParams:_) -> emit "," *> space
  (TClose, TParams:_) -> emit ")"

  (TOpen,  TInfixL _ p:xs)       -> emitIf (p < precedenceOf xs) "("
  (TSym,   TInfixL Add _:_)      -> space *> emit "+" *> space
  (TSym,   TInfixL Multiply _:_) -> space *> emit "*" *> space
  (TSym,   TInfixL Subtract _:_) -> space *> emit "-" *> space
  (TClose, TInfixL _ p:xs)       -> emitIf (p < precedenceOf xs) ")"

  (TOpen,  [Imperative])  -> pure ()
  (TOpen,  Imperative:xs) -> layout HardWrap *> indent 2 (imperativeDepth xs)
  (TSep,   Imperative:xs) -> layout HardWrap *> indent 2 (imperativeDepth xs)
  (TClose, [Imperative])  -> layout HardWrap
  (TClose, Imperative:xs) -> indent 2 (pred (imperativeDepth xs))

  (TSep, TCall:_) -> emit "."

  _ -> lift (throwError (NoTranslation el cs))

  where
    endContext times = layout HardWrap *> indent 2 (pred times)

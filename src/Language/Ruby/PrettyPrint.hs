{-# LANGUAGE Rank2Types #-}

module Language.Ruby.PrettyPrint ( printingRuby ) where

import Control.Effect
import Control.Monad.Trans (lift)
import Data.Machine

import Data.Reprinting.Scope
import Data.Reprinting.Errors
import Data.Reprinting.Operator
import Data.Reprinting.Splice
import Data.Reprinting.Token as Token

-- | Print Ruby syntax.
printingRuby :: (Member (Error TranslationError) sig, Carrier sig m, Monad m) => ProcessT m Fragment Splice
printingRuby = repeatedly (await >>= step)

step :: (Member (Error TranslationError) sig, Carrier sig m, Monad m) => Fragment -> PlanT k Splice m ()
step (Verbatim txt) = emit txt
step (New _ _ txt)  = emit txt
step (Defer el cs)  = case (el, cs) of
  (Open,  Method:_)            -> emit "def" *> space
  (Close, Method:xs)           -> endContext (imperativeDepth xs) *> emit "end"

  -- ODO: do..end vs {..} should be configurable.
  (Open,  Function:_)          -> space *> emit "do" *> space
  (Open,  Params:Function:_)  -> emit "|"
  (Close, Params:Function:_)  -> emit "|"
  (Close, Function:xs)         -> endContext (imperativeDepth xs) *> emit "end"

  -- ODO: Parens for calls are a style choice, make configurable.
  (Open,  Params:_)            -> emit "("
  (Sep,   Params:_)            -> emit "," *> space
  (Close, Params:_)            -> emit ")"

  (Open,  InfixL _ p:xs)       -> emitIf (p < precedenceOf xs) "("
  (Sym,   InfixL Add _:_)      -> space *> emit "+" *> space
  (Sym,   InfixL Multiply _:_) -> space *> emit "*" *> space
  (Sym,   InfixL Subtract _:_) -> space *> emit "-" *> space
  (Close, InfixL _ p:xs)       -> emitIf (p < precedenceOf xs) ")"

  (Open,  [Imperative])         -> pure ()
  (Open,  Imperative:xs)        -> layout HardWrap *> indent 2 (imperativeDepth xs)
  (Sep,   Imperative:xs)        -> layout HardWrap *> indent 2 (imperativeDepth xs)
  (Close, [Imperative])         -> layout HardWrap
  (Close, Imperative:xs)        -> indent 2 (pred (imperativeDepth xs))

  (Sep, Call:_)                -> emit "."

  _                              -> lift (throwError (NoTranslation el cs))

  where
    endContext times = layout HardWrap *> indent 2 (pred times)

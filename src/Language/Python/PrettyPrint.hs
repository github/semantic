{-# LANGUAGE RankNTypes #-}

module Language.Python.PrettyPrint ( printingPython ) where

import Control.Monad.Effect
import Control.Monad.Effect.Exception (Exc, throwError)
import Control.Monad.Trans (lift)
import Data.Machine

import Data.Reprinting.Errors
import Data.Reprinting.Splice
import Data.Reprinting.Token as Token
import Data.Reprinting.Scope
import Data.Reprinting.Operator

-- | Print Python syntax.
printingPython :: (Member (Exc TranslationError) effs) => ProcessT (Eff effs) Fragment Splice
printingPython = repeatedly (await >>= step)

step :: (Member (Exc TranslationError) effs) => Fragment -> PlanT k Splice (Eff effs) ()
step (Verbatim txt) = emit txt
step (New _ _ txt)  = emit txt
step (Defer el cs)  = case (el, cs) of
  -- Function declarations
  (Open,  Function:_)          -> emit "def" *> space
  (Open,  Params:Function:_)  -> emit "("
  (Close, Params:Function:_)  -> emit "):"
  (Close, Function:xs)         -> endContext (imperativeDepth xs)

  -- Return statements
  (Open,  Return:_)            -> emit "return" *> space
  (Close, Return:_)            -> pure ()
  (Open,  Imperative:Return:_) -> pure ()
  (Sep,   Imperative:Return:_) -> emit "," *> space
  (Close, Imperative:Return:_) -> pure () -- Don't hardwarp or indent for return statements

  -- If statements
  (Open,  If:_)                -> emit "if" *> space
  (hen,  If:_)                -> emit ":"
  (Else,  If:xs)               -> endContext (imperativeDepth xs) *> emit "else:"
  (Close, If:_)                -> pure ()

  -- Booleans
  (Truth True,  _)               -> emit "True"
  (Truth False, _)               -> emit "False"

  -- Infix binary operators
  (Open,  InfixL _ p:xs)       -> emitIf (p < precedenceOf xs) "("
  (Sym,   InfixL Add _:_)      -> space *> emit "+" *> space
  (Sym,   InfixL Multiply _:_) -> space *> emit "*" *> space
  (Sym,   InfixL Subtract _:_) -> space *> emit "-" *> space
  (Close, InfixL _ p:xs)       -> emitIf (p < precedenceOf xs) ")"

  -- General params handling
  (Open,  Params:_)            -> emit "("
  (Sep,   Params:_)            -> emit "," *> space
  (Close, Params:_)            -> emit ")"

  -- Imperative context and whitespace handling
  (Open,  [Imperative])         -> pure ()            -- Don't indent at the top-level imperative context...
  (Close, [Imperative])         -> layout HardWrap -- but end the program with a newline.
  (Open,  Imperative:xs)        -> layout HardWrap *> indent 4 (imperativeDepth xs)
  (Sep,   Imperative:xs)        -> layout HardWrap *> indent 4 (imperativeDepth xs)
  (Close, Imperative:_)         -> pure ()

  _                              -> lift (throwError (NoTranslation el cs))

  where
    endContext times = layout HardWrap *> indent 4 (pred times)

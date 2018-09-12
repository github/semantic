{-# LANGUAGE RankNTypes #-}

module Language.Python.PrettyPrint ( printingPython ) where

import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.Exception (Exc, throwError)
import Control.Monad.Trans (lift)
import Data.Machine
import Data.Reprinting.Errors
import Data.Reprinting.Splice
import Data.Reprinting.Token as Token

-- | Print Python syntax.
printingPython :: (Member (Exc TranslationError) effs) => ProcessT (Eff effs) Fragment Splice
printingPython = repeatedly (await >>= step)

step :: (Member (Exc TranslationError) effs) => Fragment -> PlanT k Splice (Eff effs) ()
step (Verbatim txt) = emit txt
step (New _ _ txt)  = emit txt
step (Defer el cs)  = case (el, cs) of
  -- Function declarations
  (TOpen,  TFunction:_)          -> emit "def" *> space
  (TOpen,  TParams:TFunction:_)  -> emit "("
  (TClose, TParams:TFunction:_)  -> emit "):"
  (TClose, TFunction:xs)         -> endContext (imperativeDepth xs)

  -- Return statements
  (TOpen,  TReturn:_)            -> emit "return" *> space
  (TClose, TReturn:_)            -> pure ()
  (TOpen,  Imperative:TReturn:_) -> pure ()
  (TSep,   Imperative:TReturn:_) -> emit "," *> space
  (TClose, Imperative:TReturn:_) -> pure () -- Don't hardwarp or indent for return statements

  -- If statements
  (TOpen,  TIf:_)                -> emit "if" *> space
  (TThen,  TIf:_)                -> emit ":"
  (TElse,  TIf:xs)               -> endContext (imperativeDepth xs) *> emit "else:"
  (TClose, TIf:_)                -> pure ()

  -- Booleans
  (Truth True,  _)               -> emit "True"
  (Truth False, _)               -> emit "False"

  -- Infix binary operators
  (TOpen,  TInfixL _ p:xs)       -> emitIf (p < precedenceOf xs) "("
  (TSym,   TInfixL Add _:_)      -> space *> emit "+" *> space
  (TSym,   TInfixL Multiply _:_) -> space *> emit "*" *> space
  (TSym,   TInfixL Subtract _:_) -> space *> emit "-" *> space
  (TClose, TInfixL _ p:xs)       -> emitIf (p < precedenceOf xs) ")"

  -- General params handling
  (TOpen,  TParams:_)            -> emit "("
  (TSep,   TParams:_)            -> emit "," *> space
  (TClose, TParams:_)            -> emit ")"

  -- Imperative context and whitespace handling
  (TOpen,  [Imperative])         -> pure ()            -- Don't indent at the top-level imperative context...
  (TClose, [Imperative])         -> layout HardWrap -- but end the program with a newline.
  (TOpen,  Imperative:xs)        -> layout HardWrap *> indent 4 (imperativeDepth xs)
  (TSep,   Imperative:xs)        -> layout HardWrap *> indent 4 (imperativeDepth xs)
  (TClose, Imperative:_)         -> pure ()

  _                              -> lift (throwError (NoTranslation el cs))

  where
    endContext times = layout HardWrap *> indent 4 (pred times)

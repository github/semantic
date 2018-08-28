module Language.Python.PrettyPrint ( printingPython ) where

import Control.Arrow
import Control.Monad.Effect
import Control.Monad.Effect.Exception (Exc, throwError)
import Data.Machine
import Data.Reprinting.Errors
import Data.Reprinting.Splice
import Data.Reprinting.Token as Token
import Data.Semigroup (stimes)
import Data.Sequence (Seq)

-- | Print Python syntax.
printingPython :: (Member (Exc TranslationError) effs) => ProcessT (Eff effs) Fragment Splice
printingPython = autoT (Kleisli step) ~> flattened

step :: (Member (Exc TranslationError) effs) => Fragment -> Eff effs (Seq Splice)
step (Verbatim txt) = pure $ emit txt
step (New _ _ txt)  = pure $ emit txt
step (Defer el cs)  = case (el, cs) of
  -- Function declarations
  (TOpen,  TFunction:_)         -> pure $ emit "def" <> space
  (TOpen,  TParams:TFunction:_) -> pure $ emit "("
  (TClose, TParams:TFunction:_) -> pure $ emit "):"
  (TClose, TFunction:xs)        -> pure $ endContext (depth xs)

  -- Return statements
  (TOpen,  TReturn:_) -> pure $ emit "return" <> space
  (TClose, TReturn:_) -> pure mempty
  (TOpen,  Imperative:TReturn:_) -> pure mempty
  (TSep,   Imperative:TReturn:_) -> pure $ emit "," <> space
  (TClose, Imperative:TReturn:_) -> pure mempty -- Don't hardwarp or indent for return statements

  -- If statements
  (TOpen,  TIf:_)  -> pure $ emit "if" <> space
  (TThen,  TIf:_)  -> pure $ emit ":"
  (TElse,  TIf:xs) -> pure $ endContext (depth xs) <> emit "else:"
  (TClose, TIf:xs) -> pure mempty

  -- Booleans
  (Truth True,  _) -> pure $ emit "True"
  (Truth False, _) -> pure $ emit "False"

  -- Infix binary operators
  (TOpen,  TInfixL _ p:xs)       -> emitIf (p < prec xs) "("
  (TSym,   TInfixL Add _:_)      -> pure $ space <> emit "+" <> space
  (TSym,   TInfixL Multiply _:_) -> pure $ space <> emit "*" <> space
  (TSym,   TInfixL Subtract _:_) -> pure $ space <> emit "-" <> space
  (TClose, TInfixL _ p:xs)       -> emitIf (p < prec xs) ")"

  -- General params handling
  (TOpen,  TParams:_) -> pure $ emit "("
  (TSep,   TParams:_) -> pure $ emit "," <> space
  (TClose, TParams:_) -> pure $ emit ")"

  -- Imperative context and whitespace handling
  (TOpen,  [Imperative])  -> pure mempty            -- Don't indent at the top-level imperative context...
  (TClose, [Imperative])  -> pure $ layout HardWrap -- but end the program with a newline.
  (TOpen,  Imperative:xs) -> pure $ layout HardWrap <> indent (depth xs)
  (TSep,   Imperative:xs) -> pure $ layout HardWrap <> indent (depth xs)
  (TClose, Imperative:xs) -> pure mempty -- $ indent (pred (depth xs))

  _ -> throwError (NoTranslation el cs)

  where
    emitIf predicate txt = pure $ if predicate then emit txt else mempty
    endContext times = layout HardWrap <> indent (pred times)

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
indent :: Integral b => b -> Seq Splice
indent times
  | times > 0 = stimes times (layout (Indent 4 Spaces))
  | otherwise = mempty

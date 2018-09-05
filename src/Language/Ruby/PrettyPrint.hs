module Language.Ruby.PrettyPrint ( printingRuby ) where

import Control.Arrow
import Control.Monad.Effect
import Control.Monad.Effect.Exception (Exc, throwError)
import Data.Machine
import Data.Sequence (Seq)
import Data.Reprinting.Errors
import Data.Reprinting.Splice
import Data.Reprinting.Token as Token
import Data.Semigroup (stimes)

-- | Print Ruby syntax.
printingRuby :: (Member (Exc TranslationError) effs) => ProcessT (Eff effs) Fragment Splice
printingRuby = autoT (Kleisli step) ~> flattened

step :: (Member (Exc TranslationError) effs) => Fragment -> Eff effs (Seq Splice)
step (Verbatim txt) = pure $ emit txt
step (New _ _ txt)  = pure $ emit txt
step (Defer el cs)  = case (el, cs) of
  (TOpen,  TMethod:_)  -> pure $ emit "def" <> space
  (TClose, TMethod:xs) -> pure $ endContext (depth xs) <> emit "end"

  -- TODO: do..end vs {..} should be configurable.
  (TOpen,  TFunction:_)         -> pure $ space <> emit "do" <> space
  (TOpen,  TParams:TFunction:_) -> pure $ emit "|"
  (TClose, TParams:TFunction:_) -> pure $ emit "|"
  (TClose, TFunction:xs)        -> pure $ endContext (depth xs) <> emit "end"

  -- TODO: Parens for calls are a style choice, make configurable.
  (TOpen,  TParams:_) -> pure $ emit "("
  (TSep,   TParams:_) -> pure $ emit "," <> space
  (TClose, TParams:_) -> pure $ emit ")"

  (TOpen,  TInfixL _ p:xs)       -> emitIf (p < prec xs) "("
  (TSym,   TInfixL Add _:_)      -> pure $ space <> emit "+" <> space
  (TSym,   TInfixL Multiply _:_) -> pure $ space <> emit "*" <> space
  (TSym,   TInfixL Subtract _:_) -> pure $ space <> emit "-" <> space
  (TClose, TInfixL _ p:xs)       -> emitIf (p < prec xs) ")"

  (TOpen,  [Imperative])  -> pure mempty
  (TOpen,  Imperative:xs) -> pure $ layout HardWrap <> indent (depth xs)
  (TSep,   Imperative:xs) -> pure $ layout HardWrap <> indent (depth xs)
  (TClose, [Imperative])  -> pure $ layout HardWrap
  (TClose, Imperative:xs) -> pure $ indent (pred (depth xs))

  (TSep, TCall:_) -> pure $ emit "."

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
  | times > 0 = stimes times (layout (Indent 2 Spaces))
  | otherwise = mempty

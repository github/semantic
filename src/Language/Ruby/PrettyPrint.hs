module Language.Ruby.PrettyPrint ( printingRuby ) where

import Control.Arrow
-- import Control.Monad.Trans
import Control.Monad.Effect
import Control.Monad.Effect.Exception (Exc, throwError)
import Data.Machine
import Data.Sequence (Seq)
import Data.Reprinting.Errors
import Data.Reprinting.Splice
import Data.Reprinting.Token as Token

-- | Print Ruby syntax.
printingRuby :: (Member (Exc TranslationException) effs) => ProcessT (Eff effs) Fragment Splice
printingRuby = flattened <~ autoT (Kleisli step) where

step :: (Member (Exc TranslationException) effs) => Fragment -> Eff effs (Seq Splice)
step (Verbatim txt) = pure $ emit txt
step (New _ _ txt)  = pure $ emit txt
step (Defer el cs)  = case (el, cs) of
  (TOpen,  TMethod:_)  -> pure $ emit "def" <> space
  (TClose, TMethod:xs) -> pure $ endContext (depth xs) <> emit "end"

  -- TODO: do..end vs {..} should be configurable.
  (TOpen,  TFunction:_) -> pure $ space <> emit "do" <> space
  (TOpen,  TParams:TFunction:_) -> pure $ emit "|"
  (TClose, TParams:TFunction:_) -> pure $ emit "|"
  (TClose, TFunction:xs) -> pure $ endContext (depth xs) <> emit "end"

  -- TODO: Parens for calls are a style choice, make configurable.
  (TOpen,  TParams:_) -> pure $ emit "("
  (TSep,   TParams:_) -> pure $ emit "," <> space
  (TClose, TParams:_) -> pure $ emit ")"

  (TOpen,  (TInfixL _ p):xs)   -> emitIf (p < (prec xs)) "("
  (TSep,   (TInfixL Add _):_)  -> pure $ space <> emit "+" <> space
  (TSep,   (TInfixL Mult _):_) -> pure $ space <> emit "*" <> space
  (TClose, (TInfixL _ p):xs)   -> emitIf (p < (prec xs)) ")"

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

-- Example of what writing a plan style machine looks like
-- printingRuby' :: (Member (Exc TranslationException) effs) => MachineT (Eff effs) (Is Fragment) Splice
-- printingRuby' = flattened <~ repeatedly plan where
--   plan :: (Member (Exc TranslationException) effs) =>  PlanT (Is Fragment) (Seq Splice) (Eff effs) ()
--   plan = do
--     frag <- await
--     x <- lift (step frag)
--     yield x

prec :: [Context] -> Int
prec cs = case filter isInfix cs of
  ((TInfixL _ n):_) -> n
  _ -> 0
  where isInfix (TInfixL _ _) = True
        isInfix _             = False

-- | Depth of imperative scope.
depth :: [Context] -> Int
depth = length . filter (== Imperative)

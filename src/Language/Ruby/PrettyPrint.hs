module Language.Ruby.PrettyPrint ( printingRuby ) where

import Control.Arrow
import Control.Monad.Effect
import Control.Monad.Effect.Exception (Exc, throwError)
import Data.Machine
import Data.Reprinting.Errors
import Data.Reprinting.Splice
import Data.Reprinting.Token

-- | Print Ruby syntax.
printingRuby :: (Member (Exc TranslationException) effs) => ProcessT (Eff effs) Datum Splice
printingRuby = flattened <~ autoT (Kleisli step) where
  step (Original txt)   = pure $ emit txt
  step (Insert _ _ txt) = pure $ emit txt
  step (Raw el cs)      =  case (el, cs) of
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

    (TOpen,  Imperative:[]) -> pure $ mempty
    (TOpen,  Imperative:xs) -> pure $ layout HardWrap <> indent (depth xs)
    (TSep,   Imperative:xs) -> pure $ layout HardWrap <> indent (depth xs)
    (TClose, Imperative:[]) -> pure $ layout HardWrap
    (TClose, Imperative:xs) -> pure $ indent (pred (depth xs))

    (TSep, TCall:_) -> pure $ emit "."

    _ -> throwError (NoTranslation el cs)

  endContext times = layout HardWrap <> indent (pred times)

-- | Depth of imperative scope.
depth :: [Context] -> Int
depth = length . filter (== Imperative)

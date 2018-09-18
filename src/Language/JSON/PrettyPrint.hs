module Language.JSON.PrettyPrint
  ( defaultBeautyOpts
  , defaultJSONPipeline
  , printingJSON
  , beautifyingJSON
  , minimizingJSON
  ) where

import Prologue hiding (throwError)

import Control.Monad.Effect
import Control.Monad.Effect.Exception (Exc, throwError)
import Control.Monad.Trans (lift)
import Data.Machine

import Data.Reprinting.Errors
import Data.Reprinting.Splice
import Data.Reprinting.Token

-- | Default printing pipeline for JSON.
defaultJSONPipeline :: (Member (Exc TranslationError) effs)
  => ProcessT (Eff effs) Fragment Splice
defaultJSONPipeline
  = printingJSON
  ~> beautifyingJSON defaultBeautyOpts

-- | Print JSON syntax.
printingJSON :: Monad m => ProcessT m Fragment Fragment
printingJSON = repeatedly (await >>= step) where
  step s@(Defer el cs) =
    let ins = yield . New el cs
    in case (el, listToMaybe cs) of
      (Truth True, _)      -> ins "true"
      (Truth False, _)     -> ins "false"
      (Nullity, _)         -> ins "null"

      (TOpen,  Just TList) -> ins "["
      (TClose, Just TList) -> ins "]"
      (TOpen,  Just THash) -> ins "{"
      (TClose, Just THash) -> ins "}"

      (TSep, Just TList)   -> ins ","
      (TSep, Just TPair)   -> ins ":"
      (TSep, Just THash)   -> ins ","

      _                    -> yield s
  step x = yield x

-- TODO: Fill out and implement configurable options like indentation count,
-- tabs vs. spaces, etc.
data JSONBeautyOpts = JSONBeautyOpts { jsonIndent :: Int, jsonUseTabs :: Bool }
  deriving (Eq, Show)

defaultBeautyOpts :: JSONBeautyOpts
defaultBeautyOpts = JSONBeautyOpts 2 False

-- | Produce JSON with configurable whitespace and layout.
beautifyingJSON :: (Member (Exc TranslationError) effs)
  => JSONBeautyOpts -> ProcessT (Eff effs) Fragment Splice
beautifyingJSON _ = repeatedly (await >>= step) where
  step (Defer el cs)   = lift (throwError (NoTranslation el cs))
  step (Verbatim txt)  = emit txt
  step (New el cs txt) = case (el, cs) of
    (TOpen,  THash:_) -> emit txt *> layout HardWrap *> indent 2 (hashDepth cs)
    (TClose, THash:rest) -> layout HardWrap *> indent 2 (hashDepth rest) *> emit txt
    (TSep,   TList:_)    -> emit txt *> space
    (TSep,   TPair:_)    -> emit txt *> space
    (TSep,   THash:_)    -> emit txt *> layout HardWrap *> indent 2 (hashDepth cs)
    _                    -> emit txt

-- | Produce whitespace minimal JSON.
minimizingJSON :: (Member (Exc TranslationError) effs)
  => ProcessT (Eff effs) Fragment Splice
minimizingJSON = repeatedly (await >>= step) where
  step (Defer el cs)  = lift (throwError (NoTranslation el cs))
  step (Verbatim txt) = emit txt
  step (New _ _ txt)  = emit txt

hashDepth :: [Context] -> Int
hashDepth = length . filter (== THash)

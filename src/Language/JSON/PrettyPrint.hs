module Language.JSON.PrettyPrint
  ( defaultBeautyOpts
  , defaultJSONPipeline
  , printingJSON
  , beautifyingJSON
  , minimizingJSON
  ) where

import Prologue hiding (throwError)

import Control.Effect
import Control.Effect.Error
import Control.Monad.Trans (lift)
import Data.Machine

import Data.Reprinting.Errors
import Data.Reprinting.Splice
import Data.Reprinting.Token
import Data.Reprinting.Scope

-- | Default printing pipeline for JSON.
defaultJSONPipeline :: (Member (Error TranslationError) sig, Carrier sig m, Monad m)
  => ProcessT m Fragment Splice
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

      (Open,  Just List) -> ins "["
      (Close, Just List) -> ins "]"
      (Open,  Just Hash) -> ins "{"
      (Close, Just Hash) -> ins "}"

      (Sep, Just List)   -> ins ","
      (Sep, Just Pair)   -> ins ":"
      (Sep, Just Hash)   -> ins ","

      _                    -> yield s
  step x = yield x

-- TODO: Fill out and implement configurable options like indentation count,
-- tabs vs. spaces, etc.
data JSONBeautyOpts = JSONBeautyOpts { jsonIndent :: Int, jsonUseTabs :: Bool }
  deriving (Eq, Show)

defaultBeautyOpts :: JSONBeautyOpts
defaultBeautyOpts = JSONBeautyOpts 2 False

-- | Produce JSON with configurable whitespace and layout.
beautifyingJSON :: (Member (Error TranslationError) sig, Carrier sig m, Monad m)
  => JSONBeautyOpts -> ProcessT m Fragment Splice
beautifyingJSON _ = repeatedly (await >>= step) where
  step (Defer el cs)   = lift (throwError (NoTranslation el cs))
  step (Verbatim txt)  = emit txt
  step (New el cs txt) = case (el, cs) of
    (Open,  Hash:_)    -> emit txt *> layout HardWrap *> indent 2 (hashDepth cs)
    (Close, Hash:rest) -> layout HardWrap *> indent 2 (hashDepth rest) *> emit txt
    (Sep,   List:_)    -> emit txt *> space
    (Sep,   Pair:_)    -> emit txt *> space
    (Sep,   Hash:_)    -> emit txt *> layout HardWrap *> indent 2 (hashDepth cs)
    _                    -> emit txt

-- | Produce whitespace minimal JSON.
minimizingJSON :: (Member (Error TranslationError) sig, Carrier sig m, Monad m)
  => ProcessT m Fragment Splice
minimizingJSON = repeatedly (await >>= step) where
  step (Defer el cs)  = lift (throwError (NoTranslation el cs))
  step (Verbatim txt) = emit txt
  step (New _ _ txt)  = emit txt

hashDepth :: [Scope] -> Int
hashDepth = length . filter (== Hash)

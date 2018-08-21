module Language.JSON.Translate
  ( defaultBeautyOpts
  , defaultJSONPipeline
  , translatingJSON
  , beautifyingJSON
  ) where

import Prologue

import Data.Machine
import Data.Reprinting.Splice
import Data.Reprinting.Token
import Data.Sequence

defaultJSONPipeline :: Monad m => ProcessT m Splice Splice
defaultJSONPipeline
  = translatingJSON
  ~> beautifyingJSON defaultBeautyOpts

translatingJSON :: Monad m => ProcessT m Splice Splice
translatingJSON = flattened <~ auto step where
  step :: Splice -> Seq Splice
  step s@(Unhandled el cs ) =
    let emit = splice el cs
    in case (el, listToMaybe cs) of
      (Truth True, _)  -> emit "true"
      (Truth False, _) -> emit "false"
      (Nullity, _)     -> emit "null"

      (TOpen,  Just TList) -> emit "["
      (TClose, Just TList) -> emit "]"
      (TOpen,  Just THash) -> emit "{"
      (TClose, Just THash) -> emit "}"

      (TSep, Just TList) -> emit ","
      (TSep, Just TPair) -> emit ":"
      (TSep, Just THash) -> emit ","

      _ -> pure s

  step x = pure x

-- | TODO: Fill out and implement configurable options like indentation count,
-- tabs vs. spaces, etc.
data JSONBeautyOpts = JSONBeautyOpts { jsonIndent :: Int, jsonUseTabs :: Bool }
  deriving (Eq, Show)

defaultBeautyOpts :: JSONBeautyOpts
defaultBeautyOpts = JSONBeautyOpts 2 False

beautifyingJSON :: Monad m => JSONBeautyOpts -> ProcessT m Splice Splice
beautifyingJSON _ = flattened <~ auto step where
  step :: Splice -> Seq Splice
  step s@(Insert el cs _) = case (el, listToMaybe cs) of
    (TOpen,  Just THash) -> s <| layouts [HardWrap, Indent]
    (TClose, Just THash) -> layout HardWrap |> s

    (TSep, Just TList)        -> s <| layout Space
    (TSep, Just TPair)        -> s <| layout Space
    (TSep, Just THash) -> s <| layouts [HardWrap, Indent]
    _ -> pure s
  step s = pure s

-- TODO: Could implement other steps like minimizing or uglifing.
-- minimizingJSON :: Rule eff Token (Seq Splice)
-- minimizingJSON = undefined

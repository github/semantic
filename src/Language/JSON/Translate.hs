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
  step (Insert el cs txt) = splice el cs $ case (el, listToMaybe cs) of
    (Truth True, _)  -> "true"
    (Truth False, _) -> "false"
    (Nullity, _)     -> "null"

    (Open,  Just List) -> "["
    (Close, Just List) -> "]"
    (Open,  Just Associative) -> "{"
    (Close, Just Associative) -> "}"

    (Separator, Just List) -> ","
    (Separator, Just Pair) -> ":"
    (Separator, Just Associative) -> ","

    _ -> txt

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
    (Open,  Just Associative) -> s <| directives [HardWrap, Indent]
    (Close, Just Associative) -> directive HardWrap |> s

    (Separator, Just List)        -> s <| directive Space
    (Separator, Just Pair)        -> s <| directive Space
    (Separator, Just Associative) -> s <| directives [HardWrap, Indent]
    _ -> pure s
  step s = pure s

-- TODO: Could implement other steps like minimizing or uglifing.
-- minimizingJSON :: Rule eff Token (Seq Splice)
-- minimizingJSON = undefined

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

data JSONBeautyOpts = JSONBeautyOpts { jsonPrettyPrint :: Bool }
  deriving (Eq, Show)

defaultBeautyOpts :: JSONBeautyOpts
defaultBeautyOpts = JSONBeautyOpts True

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

beautifyingJSON :: Monad m => JSONBeautyOpts -> ProcessT m Splice Splice
beautifyingJSON _ = flattened <~ auto step where
  step :: Splice -> Seq Splice
  step s@(Insert Open  (Associative:_) _) = s <| directive (HardWrap 2 Space)
  step s@(Insert Close (Associative:_) _) = directive (HardWrap 0 Space) |> s
  step x = pure x

-- TODO: Could implement other steps like minimizing or uglifing.
-- minimizingJSON :: Rule eff Token (Seq Splice)
-- minimizingJSON = undefined

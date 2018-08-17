{-# LANGUAGE UndecidableInstances #-}
module Language.JSON.Translate where

import qualified Control.Monad.Effect.Exception as Exc
import           Data.Language
import           Data.Reprinting.Token
import           Prologue
import           Reprinting.Translate
import           Reprinting.Pipeline

import           Control.Monad.Effect.Exception (Exc)
import           Control.Monad.Effect.State
import           Control.Monad.Effect.Writer


data JSONTypeSetting = JSONTypeSetting { jsonPrettyPrint :: Bool }
  deriving (Eq, Show)

prettyJSON :: JSONTypeSetting
prettyJSON = JSONTypeSetting True

instance Translation 'JSON JSONTypeSetting where
  translation _ _ content context = case (content, context) of
    (Fragment f, _) -> Right $ splice f

    (Truth True, _)  -> Right $ splice "true"
    (Truth False, _) -> Right $ splice "false"
    (Nullity, _)     -> Right $ splice "null"

    (Open, List:_)        -> Right $ splice "["
    (Open, Associative:_) -> Right $ splice "{"

    (Close, List:_)        -> Right $ splice "]"
    (Close, Associative:_) -> Right $ splice "}"

    (Separator, List:_)        -> Right $ splice ","
    (Separator, Associative:_) -> Right $ splice ","
    (Separator, Pair:_)        -> Right $ splice ":"

    _ -> Left "JSON translate failed, unknown context"

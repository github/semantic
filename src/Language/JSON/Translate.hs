module Language.JSON.Translate where

import qualified Control.Monad.Effect.Exception as Exc
import           Data.Language
import           Data.Reprinting.Token
import           Prologue
import           Reprinting.Translate

instance Translation 'JSON where
  translation _ content context = case (content, context) of
    (Fragment f, _) -> emit f

    (Truth t, _) -> emit $ if t then "true" else "false"
    (Nullity, _) -> emit "null"

    (Open, List:_)        -> emit "["
    (Open, Associative:_) -> emit "{"

    (Close, List:_)        -> emit "]"
    (Close, Associative:_) -> emit "}"

    (Separator, List:_)        -> emit ","
    (Separator, Associative:_) -> emit ","
    (Separator, Pair:_)        -> emit ":"

    _ -> Exc.throwError (Unexpected "invalid context")
